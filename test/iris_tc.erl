-module(iris_tc).
-behaviour(gen_fsm).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/0,
         authenticate/3,
         send_message/5,
         send_message_read/4,
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
          user,             %% name of the user
          pending = [],     %% list of maps to send
          recv_queue = [],  %% collect the incoming messages
          wait_for          %% {pid, ref} to wait for
         }).

start_link() ->
    gen_fsm:start_link(?MODULE, [self()], [{dbg, [trace, log]}]).

%%%
%%% High-level API
%%%

authenticate(Pid, User, Pass) ->
    Msg = #{type => <<"auth">>,
            user => list_to_binary(User),
            pass => list_to_binary(Pass)},
    send(Pid, Msg),
    wait_for_json(Pid).

send_message(Pid, User, Channel, Text, TS) ->
    Msg = #{type => <<"message">>,
            subtype => <<"send">>,
            user => list_to_binary(User),
            channel => list_to_binary(Channel),
            text => list_to_binary(Text),
            ts => list_to_binary(TS)},
    send(Pid, Msg).

send_message_read(Pid, User, Channel, TS) ->
    Msg = #{type => <<"message">>,
            subtype => <<"read">>,
            user => list_to_binary(User),
            channel => list_to_binary(Channel),
            ts => list_to_binary(TS)},
    send(Pid, Msg).

%%%
%%% Low-level API
%%%

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
        {ok, {map, Map}} ->
            {ok, Map};
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

ready({get, #{<<"type">> := <<"auth">>} = Msg}, State) ->
    User = maps:get(<<"user">>, Msg),
    {next_state, ready, State#state{user = User}};

ready({get, Message}, #state{conn = Pid, user = User} = State) ->
    Reply =
        case Message of
            #{<<"type">> := <<"message">>, <<"subtype">> := <<"incoming">>} ->
                message_received(Message, User);
            _ ->
                undefined
        end,

    case Reply of
        undefined ->
            ok;
        _ ->
            gun:ws_send(Pid, Reply)
    end,

    ?debugFmt("Message ~p State ~p", [Message, State]),

    case State of
        #state{wait_for = {FromPid, Ref}} ->
            FromPid ! {Ref, {map, Message}},
            {next_state, ready, State#state{wait_for = undefined}};
        _ ->
            %% if no one is waiting for messages, let us keep the message
            #state{recv_queue = Q} = State,
            {next_state, ready, State#state{recv_queue = Q ++ [Message]}}
    end;

ready({send, Frame}, #state{conn = Conn} = State) ->
    gun:ws_send(Conn, {text, jsx:encode(Frame)}),
    {next_state, ready, State};

ready({wait_for, From}, #state{recv_queue = []} = State) ->
    {next_state, ready, State#state{wait_for = From}};

ready({wait_for, {FromPid, Ref}}, #state{recv_queue = [Msg | Rest]} = State) ->
    FromPid ! {Ref, Msg},
    {next_state, ready, State#state{recv_queue = Rest}};

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
    Map = jsx:decode(Text, [return_maps, {labels, attempt_atom}]),
    %?debugVal(Map),
    gen_fsm:send_event(self(), {get, Map}),
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

%%%
%%% Internal functions
%%%

message_received(Message, User) ->
    Message#{<<"user">> => User,
             <<"subtype">> => <<"received">>,
             <<"to">> => maps:get(<<"user">>, Message)}.
