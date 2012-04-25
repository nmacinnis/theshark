-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post_status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-import(shark_util, [env/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

post_status(Status) ->
    TwitterUpdateUri    = env(twitter_update_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status(Status)], Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.


%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(tick, _From, State) ->
    {reply, _From, State}.

handle_cast(post, Status) ->
    {noreply, post_status(Status)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ============================================================================
%% internal
%% ============================================================================

make_status(Text) -> {status, Text}.

