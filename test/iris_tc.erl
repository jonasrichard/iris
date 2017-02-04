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

start_link() ->
    gen_fsm:start_link(?MODULE, [self()], []).

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
    {ok, connected, #{conn => Pid, parent => Parent}}.

connected({gun_up, Pid, _Proto}, StateData) ->
    gun:ws_upgrade(Pid, "/ws"),
    {next_state, connected, StateData};
connected({gun_ws_upgrade, Pid, _, _}, #{parent := _Parent} = StateData) ->
    case StateData of
        #{pending := Pending} ->
            error_logger:info_msg("Pending ~p", [Pending]),
            lists:map(
              fun(Frame) ->
                      B = jsx:encode(Frame),
                      gun:ws_send(Pid, {text, B})
              end, Pending);
        _ ->
            ok
    end,
    {next_state, ready, maps:remove(pending, StateData)};
connected({send, Frame}, StateData) ->
    case StateData of
        #{pending := Pending} ->
            Pending2 = Pending ++ [Frame],
            {next_state, connected, StateData#{pending => Pending2}};
        _ ->
            {next_state, connected, StateData#{pending => [Frame]}}
    end;
connected({wait_for, From}, StateData) ->
    {next_state, connected, StateData#{wait_for => From}}.

ready({gun_ws, _Pid, Frame}, StateData) ->
    case StateData of
        #{wait_for := {FromPid, Ref}} ->
            FromPid ! {Ref, Frame},
            NewStateData = maps:remove(wait_for, StateData),
            {next_state, ready, NewStateData};
        _ ->
            {next_state, ready, StateData}
    end;
ready({send, Frame}, #{conn := Conn} = StateData) ->
    case Frame of
        _ when is_map(Frame) ->
            gun:ws_send(Conn, jsx:encode(Frame));
        _ ->
            gun:ws_send(Conn, Frame)
    end,
    {next_state, ready, StateData};
ready({wait_for, From}, StateData) ->
    {next_state, ready, StateData#{wait_for => From}};
ready(close, #{conn := Conn} = StateData) ->
    gun:close(Conn),
    {stop, normal, StateData}.

handle_info(Msg, StateName, StateData) ->
    error_logger:info_msg("Got info ~p", [Msg]),
    gen_fsm:send_event(self(), Msg),
    {next_state, StateName, StateData}.

handle_event(_Event, Name, State) ->
    {next_state, Name, State}.

handle_sync_event(_Event, _From, Name, State) ->
    {reply, ok, Name, State}.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

