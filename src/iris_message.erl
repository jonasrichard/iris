-module(iris_message).

-export([hello/0,
         error/2,
         session/1,
         channel/1,
         message/3]).

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
      <<"id">> => Channel#channel.id}.

