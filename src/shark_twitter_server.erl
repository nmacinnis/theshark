-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-import(shark_util, [env/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

post(Status, Socket) ->
    gen_server:cast(?MODULE, {Status, Socket}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(idk, _From, State) ->
    {ok, _From, State}.

handle_cast({Status, Socket}, State) ->
    {ok, Response} = post_status(Status),
    Url = get_url_from_response(Response),
    io:format("sendan a thing ~s~n", [Url]),
    shark_irc_server:url(Url, Socket),
    {noreply, State}.

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

post_status(Status) ->
    io:format("tweetan ~s~n", [Status]),
    TwitterUpdateUri    = env(twitter_update_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status(Status)], Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

get_url_from_response(Response) ->
    {_, _, Json} = Response,
    SecondHalf = re:replace(Json, ".*id_str\":\"", "", [{return, list}]),
    Id = re:replace(SecondHalf, "\",.*", "", [{return, list}]),
    "http://twitter.com/#!/THE___SHARK/status/" ++ Id.
