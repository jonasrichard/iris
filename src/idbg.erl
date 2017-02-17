-module(idbg).

-compile(export_all).

-include("iris_db.hrl").

client() ->
    redbug:start("iris_client", [{msgs, 100}, {time, 30000}]).

rb(String) ->
    redbug:start(String, [{msgs, 100}, {time, 30000}]).

channels() ->
    mnesia:dirty_match_object(#channel{_ = '_'}).

histories() ->
    mnesia:dirty_match_object(#history{_ = '_'}).

user_channels() ->
    mnesia:dirty_match_object(#user_channel{_ = '_'}).

cursors() ->
    mnesia:dirty_match_object(#cursor{_ = '_'}).

