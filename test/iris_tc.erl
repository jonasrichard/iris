-module(iris_tc).
-behaviour(gen_fsm).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/0,
         login/2,
         hello/1,
         authenticate/3,
         send_message/5,
         send_message_read/4,
         get_read/2,
         wait_for_frame/1,
         wait_for_json/1,
         wait_for_json/2,
         wait_for_json/3,
         consume/2,
         send/2,
         send_and_wait/2,
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
          conn,                 %% the gun process
          parent,               %% the parent process
          user,                 %% name of the user
          reads = maps:new(),   %% track the read messages (map user->ts)
          pending = [],         %% list of maps to send
          recv_queue = []       %% collect the incoming messages
         }).

start_link() ->
    gen_fsm:start_link(?MODULE, [self()], [{dbg, [trace, log]}]).

%%%
%%% High-level API
%%%

login(User, Pass) ->
    {ok, Pid} = start_link(),
    hello(Pid),
    {ok, _} = authenticate(Pid, User, Pass),
    Pid.

hello(Pid) ->
    {ok, Hello} = wait_for_json(Pid),
    Hello = iris_tc_msg:hello().

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

get_read(Pid, User) ->
    gen_fsm:sync_send_all_state_event(Pid, {read_cursor, list_to_binary(User)}).

consume(_Pid, 0) ->
    ok;
consume(Pid, N) ->
    {ok, _} = wait_for_json(Pid),
    consume(Pid, N - 1).

%%%
%%% Low-level API
%%%

wait_for_frame(Pid) ->
    wait_for_frame(Pid, os:timestamp()).

wait_for_frame(Pid, TS) ->
    case timer:now_diff(os:timestamp(), TS) div 1000 of
        N when N > 5000 ->
            {error, timeout};
        _ ->
            case gen_fsm:sync_send_all_state_event(Pid, consume) of
                undefined ->
                    timer:sleep(100),
                    wait_for_frame(Pid, TS);
                Msg ->
                    {ok, Msg}
            end
    end.

wait_for_json(Pid) ->
    case wait_for_frame(Pid) of
        {ok, {map, Map}} ->
            {ok, Map};
        Other ->
            Other
    end.

wait_for_json(Pid, Type) ->
    wait_for_json2(Pid, atom_to_binary(Type, utf8)).

%% match if there is no subtype
wait_for_json2(Pid, Type) ->
    M =  wait_for_json(Pid),
    ?debugVal(M),
    case M of
        {ok, #{type := Type, subtype := _}} ->
            wait_for_json2(Pid, Type);
        {ok, #{type := Type}} = Result ->
            Result;
        _ ->
            {error, no_such_message}
    end.

wait_for_json(Pid, Type, Subtype) ->
    wait_for_json2(Pid, atom_to_binary(Type, utf8), atom_to_binary(Subtype, utf8)).

wait_for_json2(Pid, Type, Subtype) ->
    case wait_for_json(Pid) of
        {ok, #{type := Type, subtype := Subtype}} = Result ->
            Result;
        _ ->
            {error, no_such_message}
    end.

send(Pid, Frame) ->
    gen_fsm:send_event(Pid, {send, Frame}).

send_and_wait(Pid, Message) ->
    send(Pid, Message),
    {ok, Reply} = wait_for_json(Pid),
    Reply.

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
    {next_state, connected, State#state{pending = State#state.pending ++ [Frame]}}.

ready({get, #{type := <<"auth">>} = Msg}, State) ->
    User = maps:get(user, Msg),
    {next_state, ready, State#state{user = User}};

ready({get, Message}, #state{conn = Pid, user = User} = State) ->
    {Reply, State2} =
        case Message of
            #{type := <<"message">>} ->
                on_message(Message, User);
            _ ->
                {undefined, State}
        end,

    case Reply of
        undefined ->
            ok;
        _ ->
            gun:ws_send(Pid, Reply)
    end,

    %?debugFmt("Message ~p State ~p", [Message, State2]),

    #state{recv_queue = Q} = State2,
    {next_state, ready, State2#state{recv_queue = Q ++ [Message]}};

ready({send, Frame}, #state{conn = Conn} = State) ->
    gun:ws_send(Conn, {text, jsx:encode(Frame)}),
    {next_state, ready, State};

ready({consume, {FromPid, Ref}}, #state{recv_queue = Q} = State) ->
    case Q of
        [] ->
            FromPid ! {Ref, undefined},
            {next_state, ready, State#state{recv_queue = Q}};
        [Msg | Rest] ->
            FromPid ! {Ref, Msg},
            {next_state, ready, State#state{recv_queue = Rest}}
    end;

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
    gen_fsm:send_event(self(), {get, Map}),
    {next_state, ready, State};

handle_info(Msg, StateName, State) ->
    error_logger:info_msg("Got info ~p", [Msg]),
    {next_state, StateName, State}.

handle_event(_Event, Name, State) ->
    {next_state, Name, State}.

handle_sync_event(consume, _From, Name, #state{recv_queue = Q} = State) ->
    case Q of
        [] ->
            {reply, undefined, Name, State};
        [Msg | Rest] ->
            {reply, Msg, Name, State#state{recv_queue = Rest}}
    end;
handle_sync_event({read_cursor, User}, _From, Name, State) ->
    TS = maps:get(User, State#state.reads, undefined),
    {reply, TS, Name, State};
handle_sync_event(_Event, _From, Name, State) ->
    {reply, ok, Name, State}.

code_change(_OldVsn, Name, State, _Extra) ->
    {ok, Name, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%%%
%%% Internal functions
%%%

on_message(#{subtype := <<"incoming">>, user := To} = Message,
           #state{user = User} = State) ->
    Reply = Message#{user => User,
                     subtype => <<"received">>,
                     to => To},
    ?debugFmt("User ~p got incoming msg ~p", [User, maps:get(text, Message)]),
    {Reply, State};
on_message(#{subtype := <<"read">>, from := From, ts := TS},
           #state{reads = Reads} = State) ->
    Reads2 = maps:put(From, TS, Reads),
    ?debugFmt("User ~p got read receipt from ~p", [State#state.user, From]),
    {undefined, State#state{reads = Reads2}};
on_message(#{subtype := <<"stored">>, text := Text},
           #state{user = User} = State) ->
    ?debugFmt("User ~p got stored of ~p", [User, Text]),
    {undefined, State};
on_message(_, State) ->
    {undefined, State}.
