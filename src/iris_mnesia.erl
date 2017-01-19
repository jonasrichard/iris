-module(iris_mnesia).

-export([ensure_schema/0,
         ensure_table_session/0]).

-include("iris_db.hrl").

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

ensure_table_session() ->
    ok = ensure_table(session, [{ram_copies, [node()]},
                                {record_name, session},
                                {attributes, record_info(fields, session)}]).

ensure_table(Table, TabDef) ->
    case mnesia:create_table(Table, TabDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            ok;
        Other ->
            Other
    end.
