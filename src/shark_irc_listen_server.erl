-module(shark_irc_listen_server).

-behaviour(gen_server).

-export([start_link/0, go/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-import(shark_util, [env/1]).
-import(irc_basics, [process_code/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

go() ->
    gen_server:cast(?MODULE, go).
%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    go(),
    {ok, State}.

handle_call(_, _From, State) ->
    {noreply, _From, State}.

handle_cast({listen, Socket}, State) ->
    listen(Socket),
    {noreply, State};
handle_cast(go, State) ->
    initial_listen(),
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





initial_listen() ->
    Options = socket_options(),

    case gen_tcp:connect(env(irc_server), list_to_integer(env(irc_port)), Options) of
        {ok, Socket} ->
            io:format("connected to ~p:~p~n", [env(irc_server),env(irc_port)]),
            send(Socket, initial_sequence()),
            gen_server:cast(?MODULE, {listen, Socket});
        {error, Reason} ->
            io:format("failed to connect because of ~p~n", [Reason]),
            exit(self(), Reason)
    end.

send(_Socket, []) -> ok;
send(Socket, InstructionList) ->
    [Head|Tail] = InstructionList,

    case gen_tcp:send(Socket, Head ++ "\n") of
        ok ->
            io:format("sent: ~p~n", [Head]),
            case length(Tail) of
                0 -> ok;
                _ -> timer:sleep(1000)
            end,
            send(Socket, Tail);
        {error, Reason} ->
            io:format("failed to send, reason: ~p~n", [Reason])
    end.
            

listen(Socket) ->
    case gen_tcp:recv(Socket, 0, 180000) of
        {ok, Packet} ->
            process_message(Packet, Socket),
            gen_server:cast(?MODULE, {listen, Socket});
        {error, timeout} ->
            io:format("connection timed out, attempting reconnect"),
            gen_server:cast(?MODULE, go);
        {error, Reason} ->
            io:format("connection problem, reason: ~p~n", [Reason]),
            exit(self(), Reason)
    end.

process_message(Packet, Socket) ->
    io:format("~s",[Packet]),
    Tokenized = string:tokens(Packet, " "),
    [Head | _] = Tokenized,
    case string:to_lower(Head) of 
        "ping" ->
            [_, From | _] = Tokenized,
            send(Socket, [irc_commands:pong(From)]),
            shark_twitter_server:get_mentions(Socket);
        _ ->
            [_, Type | _] = Tokenized,
            parse_thing(process_code(Type), Packet, Socket)
    end.

parse_thing(Type, Packet, Socket) ->
    case Type of
        endofmotd ->
            send(Socket, [irc_commands:join(env(irc_channel))]);
        topic ->
            io:format("topic!!"),
            topic_change(Packet, Socket);
        _ ->
            ok
    end.

topic_change(Packet, Socket) ->
    Tokenized = string:tokens(Packet, " "),
    if 
        length(Tokenized) >= 4 ->
            [_, "TOPIC", _ | RemainingTokens] = Tokenized,
            Rest = lists:concat([Token ++ " " || Token <- RemainingTokens]),
            Topic = lists:sublist(Rest, 2, lists:flatlength(Rest)-3),
            io:format("~s~n", [Topic]),
            shark_twitter_server:post(Topic, Socket);
        true ->
            nil
    end.

initial_sequence() ->
    [irc_commands:password(env(irc_ident_pass)),
    irc_commands:nick(env(irc_login)),
    irc_commands:user(env(irc_login)),
    irc_commands:identify(env(irc_ident), env(irc_ident_pass))].

socket_options() ->
    [{active, false},
    {keepalive, true},
    {packet, line}].