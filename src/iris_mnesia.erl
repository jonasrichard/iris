-module(iris_mnesia).

-export([ensure_schema/0,
         ensure_tables/0,
         join/1]).

-include("iris_db.hrl").

-define(PROPS(Record), [{record_name, Record}, {attributes, record_info(fields, Record)}]).

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

join(OtherNode) ->
    Nodes = mnesia:system_info(extra_db_nodes),
    lager:info("mnesia nodes: ~p", [Nodes]),

    case lists:member(OtherNode, Nodes) of
        false ->
            lager:info("Add ~p to extra_db_nodes", [OtherNode]),
            {ok, _} = mnesia:change_config(extra_db_nodes, [OtherNode | Nodes]);
        _ ->
            ok
    end,

    lager:info("Change schema table copy"),
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _, _, _}} ->
            ok;
        Error ->
            exit(Error)
    end,

    Tables = mnesia:system_info(tables),
    Types = [{T, table_type(OtherNode, T)} || T <- Tables, t =/= schema],

    lager:info("Migrating tables ~p", [Types]),
    lists:foreach(fun migrate_table/1, Types).

migrate_table({Table, Type}) ->
    lager:info("Change ~p", [Table]),
    mnesia:add_table_copy(Table, node(), Type).

table_type(OtherNode, Table) ->
    case rpc:call(OtherNode, mnesia, table_info, [Table, storage_type]) of
        {badrpc, Error} ->
            exit(Error);
        Result ->
            Result
    end.

ensure_tables() ->
    ok = ensure_table_session(),
    ok = ensure_table_channel_proc(),
    ok = ensure_table_channel(),
    ok = ensure_table_user_channel(),
    ok = ensure_table_cursor(),
    ok = ensure_table_history(),
    ok = ensure_table_history_index().

ensure_table_session() ->
    ok = ensure_table(session, [{ram_copies, [node()]}, {index, [#session.user]} |
                                ?PROPS(session)]).

ensure_table_channel_proc() ->
    ok = ensure_table(channel_proc, [{ram_copies, [node()]} | ?PROPS(channel_proc)]).

ensure_table_channel() ->
    ok = ensure_table(channel,
                      [{disc_copies, [node()]} | ?PROPS(channel)]).

ensure_table_user_channel() ->
    ok = ensure_table(user_channel,
                      [{disc_copies, [node()]} | ?PROPS(user_channel)]).

ensure_table_cursor() ->
    ok = ensure_table(cursor, [{disc_copies, [node()]} | ?PROPS(cursor)]).

ensure_table_history() ->
    ok = ensure_table(history,
                      [{disc_only_copies, [node()]} | ?PROPS(history)]).

ensure_table_history_index() ->
    ok = ensure_table(history_index,
                      [{disc_copies, [node()]} | ?PROPS(history_index)]).

ensure_table(Table, TabDef) ->
    case mnesia:create_table(Table, TabDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            ok;
        Other ->
            Other
    end.
