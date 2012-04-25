-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, tick/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-export([post_status/1]).
%% ============================================================================
%% Module API
%% ============================================================================


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

tick() ->
    ok.
tick([]) ->
    ok.

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================


init(State) ->
    {ok, State}.

handle_call(tick, _From, State) ->
    {reply, tick(State), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================

post_status(Status) ->
    TwitterUpdateUri = shark_util:env(twitter_update_uri),
    ConsumerKey = shark_util:env(consumer_key),
    ConsumerSecret = shark_util:env(consumer_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    AccessToken = shark_util:env(access_token),
    AccessTokenSecret = shark_util:env(access_token_secret),
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status(Status)], Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

make_status(Text) -> {status, Text}.

