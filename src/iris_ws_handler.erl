-module(iris_ws_handler).

-export([init/2]).

-export([websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

-record(state, {
          client
         }).

%% The State parameter is a map, we can build the state record from their value.
init(Req, _State) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    {ok, Pid} = iris_client:start_link(websocket, self()),
    {ok, State#state{client = Pid}}.

websocket_handle(_Frame = {text, Json}, State) ->
    case process_json(Json) of
        {ok, Message} ->
            do_handle_message(Message, State),
            {ok, State};
        {error, _} ->
            {reply, {text, <<"error">>}, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({text, _Text} = Frame, State) ->
    {reply, Frame, State};
websocket_info(_Info, State) ->
    % {reply, {text, <<"ok">>}, State}
    % {stop, State}
    {ok, State}.

process_json(Json) ->
    try jsx:decode(Json, [return_maps]) of
        Data ->
            {ok, Data}
    catch
        error:badarg ->
            {error, <<"Error in JSON">>}
    end.

do_handle_message(Message, #state{client = Client}) ->
    gen_fsm:send_event(Client, Message).

