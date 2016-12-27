-module(iris_db_mnesia).

-export([init/1,
         create_table/6]).

-include("iris.hrl").

init(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    ok = mnesia:start(),
    ok.

create_table(Name, RecordInfo, Replicated, Persisted, _Index, Env) ->
    Copies = case Replicated of
                 true ->
                     maps:get(nodes, Env);
                 false ->
                     maps:get(node, Env)
             end,
    Disc = case Persisted of
               true ->
                   [{disc_copies, Copies}];
               false ->
                   []
           end,
    {atomic, ok} = mnesia:create_table(
                     Name,
                     [{attributes, RecordInfo},
                      {record_name, Name},
                      {ram_copies, Copies}] ++ Disc).

