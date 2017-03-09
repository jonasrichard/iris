#!/usr/bin/env escript

main(Args) ->
    io:format("~p~n", [Args]),
    [FileIn, FileOut | Mappings] = Args,
    {ok, Config} = file:consult(FileIn),
    NewConfig = mapping(hd(Config), Mappings),
    Out = io_lib:format("~p.", [NewConfig]),
    io:format("~p.~n", [NewConfig]),
    ok = file:write_file(FileOut, Out).

mapping(Config, []) ->
    Config;
mapping(Config, [Mapping | Rest]) ->
    [Key, Val] = string:tokens(Mapping, "="),
    Keys = tokenize(Key),
    io:format("Replacing ~p: ~p~n", [Keys, Val]),
    NewConfig = change(Config, Keys, Val),
    mapping(NewConfig, Rest).

tokenize(Dotted) ->
    Strings = string:tokens(Dotted, "."),
    [list_to_atom(String) || String <- Strings].

change(Config, [P], Value) ->
    %% Keep the type of the values
    Tuple =
      case lists:keyfind(P, 1, Config) of
          {_, Int} when is_integer(Int) ->
              {P, list_to_integer(Value)};
          {_, Atom} when is_atom(Atom) ->
              {P, list_to_atom(Value)};
          {_, Bin} when is_binary(Bin) ->
              {P, list_to_binary(Value)};
          _ ->
              {P, Value}
      end,
    lists:keyreplace(P, 1, Config, Tuple);
change(Config, [P | Rest], Value) ->
    case lists:keyfind(P, 1, Config) of
        false ->
            Config;
        {_, SubConfig} ->
            V = change(SubConfig, Rest, Value),
            lists:keyreplace(P, 1, Config, {P, V})
    end.

