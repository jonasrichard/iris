-module(iris_handler).

-export([init/2,
         handle/2]).

init(Req, State) ->
    Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                <<"ok">>,
                Req),
    {ok, Req2, State}.

handle(Req, State) ->
    {ok, Req, State}.

