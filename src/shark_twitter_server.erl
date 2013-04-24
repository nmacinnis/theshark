-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post/2, get_mentions/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-import(shark_util, [env/1]).

-record(state, {socket, mention_id}).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

post(Status, Socket) ->
    gen_server:cast(?MODULE, {Status, Socket}).

get_mentions(Socket) ->
    gen_server:cast(?MODULE, {mentions, Socket}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(_, _From, State) ->
    {ok, _From, State}.

handle_cast({mentions, Socket}, State) ->
    {ok, Response} = get_mentions(),
    Text = get_mention_from_response(Response),
    shark_irc_talk_server:say(Text, Socket),
    {noreply, State};
handle_cast({Status, Socket}, State) ->
    {ok, Response} = post_status(Status),
    Url = get_url_from_response(Response),
    shark_irc_talk_server:say(Url, Socket),
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

get_mentions_args() -> 
    [
        {count, "1"},
        {include_entities, "false"}
    ].

post_status(Status) ->
    TwitterUpdateUri    = env(twitter_update_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status(Status)], Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

get_mentions() ->
    TwitterMentionsUri  = env(twitter_mentions_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:get(TwitterMentionsUri, get_mentions_args(), Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

get_url_from_response(Response) ->
    {_, _, Json} = Response,
    {Terms} = json_eep:json_to_term(Json),
    {_, IdBinary} = lists:keyfind(<<"id_str">>, 1, Terms),
    Id = binary_to_list(IdBinary),
    "http://twitter.com/#!/THE___SHARK/status/" ++ Id.

get_mention_from_response(Response) ->
    {_, _, Json} = Response,
    [{Terms}] = json_eep:json_to_term(Json),
    {_, TextBinary} = lists:keyfind(<<"text">>, 1, Terms),
    Text = binary_to_list(TextBinary),
    {_, {UserBinary}} = lists:keyfind(<<"user">>, 1, Terms),
    {_, ScreenNameBinary} = lists:keyfind(<<"screen_name">>, 1, UserBinary),
    ScreenName = binary_to_list(ScreenNameBinary),
    Text ++ " ~ " ++ ScreenName.
