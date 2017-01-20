-module(iris_mod_register).

-export([start_link/1,
         init/2,
         authenticate/2,
         handle_message/3]).

-include("iris.hrl").

-define(USERNAME, <<"user">>).
-define(PASSWORD, <<"password">>).

start_link(Opts) ->
    proc_lib:start_link(?MODULE, init, [Opts, self()]).

init(Opts, Parent) ->
    lager:info("Starting iris_mod_register with ~p", [Opts]),
    iris_db:create_table(user, record_info(fields, user), true),
    iris_hook:add(authenticate, ?MODULE, authenticate, 10),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop().

loop() ->
    receive
        _ ->
            loop()
    end.

authenticate(User, Pass) ->
    case {User, Pass} of
        {<<"user", _/binary>>, _} ->
            {stop, true};
        _ ->
            %% Give the chance to other members of the hook
            %% to authenticate the user
            ok
    end.

handle_message(_From, _To, Message) ->
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

