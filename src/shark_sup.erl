-module(shark_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 2, 60},
        [
            {shark_twitter_server,
              {shark_twitter_server, start_link, []},
              permanent, brutal_kill, worker, [shark_twitter_server]},
            {shark_memer_server,
              {shark_memer_server, start_link, []},
              permanent, brutal_kill, worker, [shark_memer_server]},
            {shark_irc_listen_server,
              {shark_irc_listen_server, start_link, []},
              permanent, brutal_kill, worker, [shark_irc_listen_server]},
            {shark_irc_talk_server,
              {shark_irc_talk_server, start_link, []},
              permanent, brutal_kill, worker, [shark_irc_talk_server]}
        ]
    }
}.
