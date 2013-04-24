-module(shark_irc_talk_server).

-behaviour(gen_server).

-export([start_link/0, url/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-import(shark_util, [env/1]).
-import(irc_basics, [process_code/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

url(Url, Socket) ->
    gen_server:cast(?MODULE, {url, Url, Socket}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(_, _From, State) ->
    {noreply, _From, State}.

handle_cast({url, Url, Socket}, State) ->
    io:format("sayin a thing ~s~n", [Url]),
    send(Socket, [irc_commands:say(env(irc_channel), Url)]),
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
