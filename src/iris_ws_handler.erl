-module(iris_ws_handler).

-export([init/2]).

-export([websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle(Frame = {text, Json}, State) ->
    case process_json(Json) of
        {ok, Data} ->
            % handle incoming message
            {reply, {text, <<"ok">>}, State};
        {error, _} ->
            {ok, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

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

