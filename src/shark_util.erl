-module(shark_util).

-export([env/1]).

env(Variable) ->
    case application:get_env(shark_app, Variable) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.

