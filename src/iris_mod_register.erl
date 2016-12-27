-module(iris_mod_register).

-export([init/1,
         handle_message/3]).

-include("iris.hrl").

-define(USERNAME, <<"user">>).
-define(PASSWORD, <<"password">>).

init(Opts) ->
    iris_db:create_table(user, record_info(fields, user), true, undefined, undefined).

handle_message(From, To, Message) ->
    try
        User = maps:get(?USERNAME, Message),
        Pass = maps:get(?PASSWORD, Message),
        ok = register_user(User, Pass)
    catch
        _:_ ->
            {error, <<"Error during registration">>}
    end.

register_user(User, Pass) ->
    iris_db:insert({user, User, Pass}).

