-module(shark_irc_server).

-behaviour(gen_server).

-export([start_link/0, url/2, go/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([loop/1]).

-import(shark_util, [env/1]).
-import(irc_basics, [process_code/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

url(Url, Socket) ->
    gen_server:call(?MODULE, {Url, Socket}).

go() ->
    gen_server:cast(?MODULE, go).
%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    go(),
    {ok, State}.

handle_call({Url, Socket}, _From, State) ->
    io:format("sayin a thing ~s~n", [Url]),
    send(Socket, [irc_commands:say(env(irc_channel), Url)]),
    {reply, _From, State}.

handle_cast({accepted, _Pid, Socket}, State) ->
    spawn_loop_proc(Socket),
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
            spawn_loop_proc(Socket);
        {error, Reason} ->
            Reason
    end.

send(_Socket, []) -> ok;
send(Socket, InstructionList) ->
    [Head|Tail] = InstructionList,

    ok = gen_tcp:send(Socket, Head ++ "\n"),
    io:format("sent: ~p~n", [Head]),

    case length(Tail) of
        0 -> ok;
        _ -> timer:sleep(1000)
    end,

    send(Socket, Tail).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            process_message(Packet, Socket);
            %%gen_server:cast(Server, {received, self(), Socket});
        {error, Reason} ->
            io:format(Reason)
    end,
    loop(Socket).

spawn_loop_proc(Socket) ->
    proc_lib:spawn(?MODULE, loop, [Socket]),
    Socket.

process_message(Packet, Socket) ->
    io:format("~s",[Packet]),
    Tokenized = string:tokens(Packet, " "),
    [Head | _] = Tokenized,
    case string:to_lower(Head) of 
        "ping" ->
            [_, From | _] = Tokenized,
            parse_thing(ping, From, Socket);
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
        ping ->
            send(Socket, [irc_commands:pong(Packet)]);
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
