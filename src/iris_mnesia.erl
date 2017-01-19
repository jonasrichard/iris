-module(iris_mnesia).

-export([ensure_schema/0]).

ensure_schema() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            application:stop(mnesia),
            Node = node(),
            case mnesia:create_schema([node()]) of
                ok ->
                    application:start(mnesia, permanent);
                {error, {Node, {already_exists, Node}}} ->
                    application:start(mnesia, permanent);
                Error ->
                    exit(Error)
            end;
        N ->
            lager:info("mnesia already has extra db nodes: ~p", [N])
    end.
