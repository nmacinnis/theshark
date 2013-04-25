-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post/1, get_mentions/0, update_socket/2, update_mention_id/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-import(shark_util, [env/1]).

-include("record.hrl").

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    State = #state{},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

post(Status) ->
    gen_server:cast(?MODULE, {status, Status}).

get_mentions() ->
    gen_server:cast(?MODULE, {mentions}).

update_socket(From, Socket) ->
    gen_server:call(From, {update_socket, Socket}).

update_mention_id(From, MentionId) ->
    gen_server:call(From, {update_mention_id, MentionId}).
%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call({update_socket, Socket}, _From, State) ->
    UpdatedState = State#state{socket = Socket},
    {ok, _From, UpdatedState};
handle_call({update_mention_id, MentionId}, _From, State) ->
    UpdatedState = State#state{mention_id = MentionId},
    {ok, _From, UpdatedState};
handle_call(_, _From, State) ->
    {ok, _From, State}.

handle_cast(mentions, State) ->
    {ok, Response} = get_latest_mention(State),
    case get_mention_from_response(Response) of
        noupdate -> noupdate;
        Text -> shark_irc_talk_server:say(Text, State#state.socket)
    end,
    {noreply, State};
handle_cast({status, Status}, State) ->
    {ok, Response} = post_status(Status),
    Url = get_url_from_response(Response),
    shark_irc_talk_server:say(Url, State#state.socket),
    {noreply, State};
handle_cast(_, State) ->
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


get_mentions_args(noid) -> 
    [
        {count, "1"},
        {include_entities, "false"}
    ];
get_mentions_args(SinceId) ->
    get_mentions_args(noid) ++ {since_id, SinceId}.

post_status(Status) ->
    TwitterUpdateUri    = env(twitter_update_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:post(TwitterUpdateUri, [make_status(Status)], Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

get_latest_mention(State) ->
    TwitterMentionsUri  = env(twitter_mentions_uri),
    ConsumerKey         = env(consumer_key),
    ConsumerSecret      = env(consumer_secret),
    AccessToken         = env(access_token),
    AccessTokenSecret   = env(access_token_secret),
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    OauthResponse = oauth:get(TwitterMentionsUri, get_mentions_args(State#state.mention_id), Consumer, AccessToken, AccessTokenSecret),
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
    case lists:keyfind(<<"id_str">>, 1, Terms) of
        false -> noupdate;
        _ -> parse_mention_terms(Terms)
    end.

parse_mention_terms(Terms) ->
    {_, IdBinary} = lists:keyfind(<<"id_str">>, 1, Terms),
    Id = binary_to_list(IdBinary),
    update_mention_id(self(), Id),
    {_, TextBinary} = lists:keyfind(<<"text">>, 1, Terms),
    Text = binary_to_list(TextBinary),
    {_, {UserBinary}} = lists:keyfind(<<"user">>, 1, Terms),
    {_, ScreenNameBinary} = lists:keyfind(<<"screen_name">>, 1, UserBinary),
    ScreenName = binary_to_list(ScreenNameBinary),
    Text ++ " ~ " ++ ScreenName.
