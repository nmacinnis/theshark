-module(shark_irc_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-import(shark_util, [env/1]).
-import(irc_basics, [process_code/1]).

-export([go/0,send/2]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(tick, _From, State) ->
    {reply, _From, State}.

handle_cast(unsure, Unsure) ->
    {noreply, Unsure}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ============================================================================
%% internal
%% ============================================================================




go() ->
    ets:new(chanstate,[named_table]),
    initial_listen().

initial_listen() ->
    Options = socket_options(),

    case gen_tcp:connect(env(irc_server), list_to_integer(env(irc_port)), Options) of
        {ok, Socket} ->
            io:format("connected to ~p:~p~n", [env(irc_server),env(irc_port)]),
            loop(Socket);
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
            process_message(Packet, Socket),
            loop(Socket);
        {error, Reason} ->
            io:format(Reason)
    end.

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
        pubnotice ->
            check_notice(Socket, Packet);
        kick ->
            send(Socket, [irc_commands:join(env(irc_channel)),irc_commands:list_names(env(irc_channel))]);
        endofmotd ->
            send(Socket, [irc_commands:join(env(irc_channel))]);
        join ->
            send(Socket, %[irc_commands:say(env(irc_channel), "Hey what's up"),
                [irc_commands:list_names(env(irc_channel))]);
        nick ->
            send(Socket, [irc_commands:list_names(env(irc_channel))]);
        part ->
            send(Socket, [irc_commands:list_names(env(irc_channel))]);
        privmsg ->
            ok;
        topic ->
            io:format("topic!!"),
            topic_change(Packet);
        ping ->
            send(Socket, [irc_commands:pong(Packet)]);
        namreply ->
            handle_names(Packet);
        endofnames ->
            end_of_names();
        _ ->
            ok
    end.

handle_names(Packet) ->
    TempKey = env(irc_channel) ++ "_temp",
    NameList = extract_names(Packet),
    case ets:lookup(chanstate, TempKey) of
        [] -> 
            ets:insert(chanstate, {TempKey, NameList});
        [{TempKey, CurrNames}] ->
            ets:insert(chanstate, {TempKey, NameList ++ CurrNames})
    end.
            
extract_names(Packet) ->
    [_Server, _Code, _Nick, _Equal, _Channel | Names] = 
        string:tokens(Packet, "\r\n :@+"),
    Names.

end_of_names() ->
    TempKey = env(irc_channel) ++ "_temp",
    case ets:lookup(chanstate, TempKey) of 
        [] -> 
            skip;
        [{TempKey, Names}] ->
            ets:insert(chanstate, {env(irc_channel), Names}),
            ets:delete(chanstate, TempKey)
    end.

check_notice(Socket, Packet) ->
    [_Server, _Type, From, _Message | _Tail] = string:tokens(Packet, " "),
    case From of
        "*" -> send(Socket, initial_sequence());
        _ -> ok
    end.

topic_change(Packet) ->
    Tokenized = string:tokens(string:to_lower(Packet), " "),
    if 
        length(Tokenized) >= 4 ->
            [From, "topic", Channel | Rest] = Tokenized,
            io:format("~s~n", [Rest]);
        true ->
            nil
    end.



%% spam this until we end up in a channel, this is really
%% ghetto but its not immediately clear to me that theres
%% a super nicer way to handle this
    
initial_sequence() ->
    [irc_commands:password(env(irc_ident_pass)),
    irc_commands:nick(env(irc_login)),
    irc_commands:user(env(irc_login)),
    irc_commands:identify(env(irc_ident), env(irc_ident_pass))].

socket_options() ->
    [{active, false},
    {keepalive, true},
    {packet, line}].
