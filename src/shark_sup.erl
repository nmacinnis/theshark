-module(shark_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 60},
        [{theshark, {theshark, start_link, []},
          permanent, brutal_kill, worker, [theshark]}]}}.
