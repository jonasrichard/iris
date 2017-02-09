-module(iris_tc).
-behaviour(gen_fsm).

-export([start_link/0,
         wait_for_frame/1,
         wait_for_json/1,
         send/2,
         close/1]).

-export([init/1,
         handle_info/3,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         terminate/3,
         connected/2,
         ready/2]).

-record(state, {
          conn,             %% the gun process
          parent,           %% the parent process
          pending = [],     %% list of maps to send
          wait_for          %% {pid, ref} to wait for
         }).

start_link() ->
    gen_fsm:start_link(?MODULE, [self()], [{dbg, [trace, log]}]).

wait_for_frame(Pid) ->
    Ref = erlang:make_ref(),
    gen_fsm:send_event(Pid, {wait_for, {self(), Ref}}),
    receive
        {Ref, Frame} ->
            {ok, Frame}
    after 5000 ->
              {error, timeout}
    end.

wait_for_json(Pid) ->
    case wait_for_frame(Pid) of
        {ok, {text, Text}} ->
            {ok, jsx:decode(Text, [return_maps])};
        Other ->
            Other
    end.

send(Pid, Frame) ->
    gen_fsm:send_event(Pid, {send, Frame}).

close(Pid) ->
    gen_fsm:send_event(Pid, close).

%%%
%%% gen_fsm callbacks and states
%%%

init([Parent]) ->
    {ok, Pid} = gun:open("localhost", 8080),
    erlang:monitor(process, Pid),
    {ok, connected, #state{conn = Pid, parent = Parent}}.

connected({send, Frame}, State) ->
    {next_state, connected, State#state{pending = State#state.pending ++ [Frame]}};

connected({wait_for, From}, State) ->
    {next_state, connected, State#state{wait_for = From}}.

ready({text, Text}, #state{conn = _Pid} = State) ->
    %% TODO: here should handle decoded jsons
    %%       once we get it, decode it!
    %% gun:ws_send(Pid, jsx:encode()),
    case State of
        #state{wait_for = {FromPid, Ref}} ->
            FromPid ! {Ref, {text, Text}},
            {next_state, ready, State#state{wait_for = undefined}};
        _ ->
            {next_state, ready, State}
    end;

ready({send, Frame}, #state{conn = Conn} = State) ->
    gun:ws_send(Conn, {text, jsx:encode(Frame)}),
    {next_state, ready, State};

ready({wait_for, From}, State) ->
    {next_state, ready, State#state{wait_for = From}};

ready(close, #state{conn = Conn} = State) ->
    gun:close(Conn),
    {stop, normal, State}.

handle_info({gun_up, Pid, http}, connected, #state{conn = Pid} = State) ->
    gun:ws_upgrade(Pid, "/ws"),
    {next_state, connected, State};

handle_info({gun_ws_upgrade, Pid, ok, _}, connected, #state{conn = Pid} = State) ->
    lists:foreach(
      fun(Frame) ->
              gun:ws_send(Pid, {text, jsx:encode(Frame)})
      end, State#state.pending),
    {next_state, ready, State#state{pending = []}};

handle_info({gun_ws, _Pid, {text, Text}}, ready, State) ->
    gen_fsm:send_event(self(), {text, Text}),
    {next_state, ready, State};

handle_info(Msg, StateName, State) ->
    error_logger:info_msg("Got info ~p", [Msg]),
    {next_state, StateName, State}.

handle_event(_Event, Name, State) ->
    {next_state, Name, State}.

handle_sync_event(_Event, _From, Name, State) ->
    {reply, ok, Name, State}.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

