-module(shark_twitter_server).

-behaviour(gen_server).

-export([start_link/0, post/1, get_mentions/0, update_socket/1, update_mention_id/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-export([print_irl/1]).

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
    io:format("updated socket is now ~w~n", [Socket]),
    UpdatedState = State#state{socket = Socket},
    {reply, _From, UpdatedState};
handle_call({update_mention_id, MentionId}, _From, State) ->
    io:format("latest mention id is now ~w~n", [MentionId]),
    UpdatedState = State#state{mention_id = MentionId},
    {reply, _From, UpdatedState}.

handle_cast(mentions, State) ->
    case get_latest_mention(State) of
        {ok, Response} ->
            case get_mention_from_response(Response) of
                noupdate -> {noreply, State};
                {error, _} -> {noreply, State};
                {Id, Text} ->
                    case State#state.mention_id of
                        noid ->
                            shh;
                        _ ->
                            shark_irc_talk_server:say(Text),
                            print_irl(Text)
                    end,
                    {noreply, State#state{mention_id = Id}}
            end;
        _ -> {noreply, State}
    end;
handle_cast({status, Status}, State) ->
    {ok, Response} = post_status(Status),
    case get_url_from_response(Response) of
        {ok, Url} ->
            shark_irc_talk_server:say(Url);
        {error, _} ->
            pass
    end,
    print_irl(Status),
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
    get_mentions_args(noid) ++ [{since_id, SinceId}].

post_status(Status) ->
    io:format("oh shit tweetin, damn: ~p~n", [Status]),
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
    io:format("requesting the latest mention from twitter~n"),
    OauthResponse = oauth:get(TwitterMentionsUri, get_mentions_args(State#state.mention_id), Consumer, AccessToken, AccessTokenSecret),
    OauthResponse.

get_url_from_response(Response) ->
    case extract_json_from_response(Response) of
        {ok, Json} ->
            extract_url_from_json(Json);
        {error, Error} ->
            {error, Error}
    end.

extract_json_from_response(Response) ->
    try erlang:element(3, Response) of
        Json ->
            {ok, Json}
    catch
        error:Error ->
            io:format("problematic response :/ ->~n~p~n", [Response]),
            io:format("error ->~n~p~n", [Error]),
            {error, Error}
    end.

extract_url_from_json(Json) ->
    io:format("json! : ->~n~p~n", [Json]),
    try json_eep:json_to_term(Json) of
        {Terms} ->
            {_, IdBinary} = lists:keyfind(<<"id_str">>, 1, Terms),
            Id = binary_to_list(IdBinary),
            {ok, "http://twitter.com/#!/" ++ env(twitter_user) ++ "/status/" ++ Id}
    catch
        error:Error ->
            io:format("problematic json :/ ->~n~p~n", [Json]),
            io:format("error ->~n~p~n", [Error]),
            {error, Error}
    end.

get_mention_from_response(Response) ->
    case extract_json_from_response(Response) of
        {ok, Json} ->
            extract_mention_from_json(Json);
        {error, Error} ->
            {error, Error}
    end.

extract_mention_from_json(Json) ->
    io:format("json! : ->~n~p~n", [Json]),
    try json_eep:json_to_term(Json) of
        [{Terms}] ->
            case lists:keyfind(<<"id_str">>, 1, Terms) of
                false -> noupdate;
                _ -> parse_mention_terms(Terms)
            end;
        [] ->
            noupdate
    catch
        error:Error ->
            io:format("problematic json :/ ->~n~p~n", [Json]),
            io:format("error ->~n~p~n", [Error]),
            {error, Error}
    end.

parse_mention_terms(Terms) ->
    io:format("got a mention from twitter~n"),
    {_, IdBinary} = lists:keyfind(<<"id_str">>, 1, Terms),
    Id = binary_to_list(IdBinary),
    {_, TextBinary} = lists:keyfind(<<"text">>, 1, Terms),
    Text = binary_to_list(TextBinary),
    {_, {UserBinary}} = lists:keyfind(<<"user">>, 1, Terms),
    {_, ScreenNameBinary} = lists:keyfind(<<"screen_name">>, 1, UserBinary),
    ScreenName = binary_to_list(ScreenNameBinary),
    {Id, Text ++ " ~ " ++ ScreenName}.

print_irl(Text) ->
    io:format("attempting to print irl~n~p~n", [Text]),
    % Stuff = os:cmd("python /home/pi/workspace/Adafruit-Raspberry-Pi-Python-Code/Adafruit_CharLCDPlate/display_msg.py '" ++ EscapedText ++ "'"),
    % LcdUrl = env(meme_captain_url),
    LcdUrl = "http://192.168.1.107:8090/new?new_message=",
    Url = LcdUrl ++ http_uri:encode(Text),
    Response = httpc:request(Url),
    case Response of
        {ok, Stuff} ->
            io:format("the stuff was ~n~p~n", [Stuff]),
            Url;
        {error, _} ->
            dang
    end.
