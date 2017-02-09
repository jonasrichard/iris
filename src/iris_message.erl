-module(iris_message).

-export([hello/0,
         error/2,
         session/1,
         channel/1,
         message/3]).

-export([parse/1]).

-include("iris_db.hrl").

hello() ->
    #{<<"type">> => <<"hello">>}.

error(Code, Message) ->
    #{<<"type">> => <<"error">>,
      <<"error">> => #{
          <<"code">> => Code,
          <<"msg">> => Message}}.

message(Recipient, Channel, Text) ->
    #{<<"type">> => <<"message">>,
      <<"channel">> => Channel,
      <<"user">> => Recipient,
      <<"text">> => Text,
      <<"ts">> => iris_utils:ts()}.

session(SessionId) ->
    #{<<"type">> => <<"session">>,
      <<"sessionId">> => SessionId}.

channel(Channel) ->
    #{<<"type">> => <<"channel">>,
      <<"id">> => Channel#channel.id,
      <<"name">> => Channel#channel.name,
      <<"members">> => Channel#channel.members,
      <<"created_ts">> => Channel#channel.created_ts,
      <<"last_ts">> => Channel#channel.last_ts}.

parse(#{<<"type">> := <<"message">>} = Json) ->
    parse_message(Json);
parse(#{<<"type">> := <<"channel.create">>} = Json) ->
    map_key_to_atom(Json, [<<"type">>, <<"name">>, <<"user">>, <<"invitees">>]);
parse(#{<<"type">> := <<"channel.list">>}) ->
    #{type => <<"channel.list">>};
parse(#{<<"type">> := <<"channel.history">>} = Json) ->
    map_key_to_atom(Json, [<<"type">>, <<"channel">>,
                           <<"start_ts">>, <<"last_ts">>]).

parse_message(#{<<"subtype">> := <<"send">>} = Map) ->
    map_key_to_atom(Map, [<<"type">>, <<"subtype">>, <<"user">>, <<"text">>,
                          <<"channel">>, <<"ts">>]);
parse_message(#{<<"subtype">> := <<"received">>} = Map) ->
    map_key_to_atom(Map, [<<"type">>, <<"subtype">>, <<"user">>,
                          <<"channel">>, <<"ts">>]);
parse_message(#{<<"subtype">> := <<"read">>} = Map) ->
    map_key_to_atom(Map, [<<"type">>, <<"subtype">>, <<"user">>, <<"to">>,
                          <<"channel">>, <<"ts">>]).

map_key_to_atom(Map, Keys) ->
    KL = maps:fold(
           fun(Key, Value, Acc) ->
                   case lists:member(Key, Keys) of
                       true ->
                           [{binary_to_atom(Key, utf8), Value} | Acc];
                       false ->
                           Acc
                   end
           end,
           [],
           Map),
    maps:from_list(KL).
