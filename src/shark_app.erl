-module(shark_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([do/0]).

start(_Type, _StartArgs) ->
    case shark_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) ->
    ok.

do() ->
    shark_twitter_server:do().
