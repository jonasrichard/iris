-module(iris_sm).

-export([init/1,
         teardown/1]).

-export([save_session/2,
         delete_session/1,
         get_session/1,
         get_session_by_user/1]).

-include("iris_db.hrl").

%%%
%%% API functions
%%%

save_session(Pid, User) ->
    case get_session_by_user(User) of
        {error, not_found} ->
            %% there is no previous session
            ok;
        {ok, #session{id = OtherId, pid = OtherPid}} ->
            %% TODO: check if other process is alive
            OtherPid ! kick_out,
            mnesia:dirty_delete(session, OtherId)
    end,
    Id = get_id(),
    mnesia:dirty_write(session, #session{id = Id,
                                         pid = Pid,
                                         user = User}),
    Id.

delete_session(Id) ->
    %% TODO check ok and log if it is different
    mnesia:dirty_delete(session, Id).

get_session(Id) ->
    case mnesia:dirty_read(session, Id) of
        [] ->
            {error, not_found};
        [#session{} = Session] ->
            {ok, Session}
    end.

get_session_by_user(User) ->
    case mnesia:dirty_index_read(session, User, #session.user) of
        [] ->
            {error, not_found};
        [Session] ->
            {ok, Session}
    end.

%%%
%%% Module callbacks
%%%

init(_Opts) ->
    ok.

teardown(_Opts) ->
    ok.

%%%
%%% Internal function
%%%

get_id() ->
    erlang:phash2({node(), os:timestamp()}, 16#ffffffff).
