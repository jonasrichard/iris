-module(iris_sm).

-export([init/1,
         teardown/1]).

-export([save_session/2,
         get_session/1]).

-include("iris_db.hrl").

%%%
%%% API functions
%%%

save_session(Pid, User) ->
    case mnesia:dirty_index_read(session, User, #session.user) of
        [] ->
            %% there is no previous session
            ok;
        [#session{id = OtherId, pid = OtherPid}] ->
            %% TODO: check if other process is alive
            OtherPid ! kick_out,
            mnesia:dirty_delete(session, OtherId)
    end,
    Id = get_id(),
    mnesia:dirty_write(session, #session{id = Id,
                                         pid = Pid,
                                         user = User}),
    Id.

get_session(Id) ->
    case mnesia:dirty_read(session, Id) of
        [] ->
            {error, not_found};
        [#session{} = Session] ->
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
