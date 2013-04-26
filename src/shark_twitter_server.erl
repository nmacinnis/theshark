-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post/1, get_mentions/0, update_socket/1, update_mention_id/1]).
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
    io:format("k dude told me get mentions what we got"),
    gen_server:cast(?MODULE, mentions).

update_socket(Socket) ->
    gen_server:call(?MODULE, {update_socket, Socket}).

update_mention_id(MentionId) ->
    gen_server:call(?MODULE, {update_mention_id, MentionId}).
%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call({update_socket, Socket}, _From, State) ->
    io:format("updated socket is now ~w", [Socket]),
    UpdatedState = State#state{socket = Socket},
    {reply, _From, UpdatedState};
handle_call({update_mention_id, MentionId}, _From, State) ->
    io:format("latest mention id is now ~w", [MentionId]),
    UpdatedState = State#state{mention_id = MentionId},
    {reply, _From, UpdatedState}.

handle_cast(mentions, State) ->
    io:format("aight gettin some mentions here we goooo"),
    {ok, Response} = get_latest_mention(State),
    case get_mention_from_response(Response) of
        noupdate -> {noreply, State};
        {Id, Text} ->
            io:format("ok sayin some stuff to talkbot"),
            shark_irc_talk_server:say(Text, State#state.socket),
            {noreply, State#state{mention_id = Id}}
    end;
handle_cast({status, Status}, State) ->
    {ok, Response} = post_status(Status),
    Url = get_url_from_response(Response),
    shark_irc_talk_server:say(Url, State#state.socket),
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
    io:format("tryna find a mention"),
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
    io:format("got some terms woop woop"),
    {_, IdBinary} = lists:keyfind(<<"id_str">>, 1, Terms),
    Id = binary_to_list(IdBinary),
    {_, TextBinary} = lists:keyfind(<<"text">>, 1, Terms),
    Text = binary_to_list(TextBinary),
    {_, {UserBinary}} = lists:keyfind(<<"user">>, 1, Terms),
    {_, ScreenNameBinary} = lists:keyfind(<<"screen_name">>, 1, UserBinary),
    ScreenName = binary_to_list(ScreenNameBinary),
    {Id, Text ++ " ~ " ++ ScreenName}.
