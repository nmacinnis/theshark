-module(shark_twitter_server).
-export([do/0, quote_text/1, make_status/1, make_status_request/1]).

-behaviour(gen_server).

-export([start_link/0, tick/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-export([do/0]).
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

do() ->
    TwitterUser = env(twitter_user),
    io:format("~s~n", [TwitterUser]),
    TwitterPass = env(twitter_pass),
    io:format("~s~n", [TwitterPass]),
    TwitterUpdateUri = env(twitter_update_uri),
    io:format("~s~n", [TwitterUpdateUri]),
    TwitterContentType = "application/x-www-form-urlencoded",
    ConsumerKey = env(consumer_key),
    io:format("~s~n", [ConsumerKey]),
    ConsumerSecret = env(consumer_secret),
    io:format("~s~n", [ConsumerSecret]),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    AccessToken = env(access_token),
    io:format("~s~n", [AccessToken]),
    AccessTokenSecret = env(access_token_secret),
    io:format("~s~n", [AccessTokenSecret]),
    IrcServer = env(irc_server),
    IrcLogin = env(irc_login),
    IrcIdent = env(irc_ident),
    IrcIdentPass = env(ird_ident_pass),
    IrcChannel = env(irc_channel),
    io:format("~n~n~n~n", []),
    %%oauth:uri_params_encode(oauth:sign("POST", TwitterUpdateUri, [make_status("Status!")], Consumer, AccessToken, AccessTokenSecret)).
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status("Status!")], Consumer, AccessToken, AccessTokenSecret),
    %%io:format(OauthResponse).
    OauthResponse.

quote_text(Text) ->
    "\"" ++ Text ++ "\"".

make_status(Text) ->
    "{" ++ quote_text("status") ++ ":" ++ quote_text(Text) ++ "}".

make_status_request(Status) ->
    %%io:format(Status),
    Headers = [],
    Body = Status,
    {url, Headers, contenttype, Body}.

join([]) -> "";
join([W|Ws]) -> join(Ws, W).

join([], S) -> S;
join([W], S) -> join([], S ++ " and " ++ W);
join([W|Ws], S) -> join(Ws, S ++ ", " ++ W).    

env(Variable) ->
    case application:get_env(shark_app, Variable) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.

