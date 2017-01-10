-module(iris_msg_json).

-export([error_msg/1,
         error_msg/2]).

error_msg(Description) ->
    #{type => error,
      error => #{
        code => 0,
        msg => Description}}.

error_msg(Description, Code) ->
    #{type => error,
      error => #{
        code => Code,
        msg => Description}}.
