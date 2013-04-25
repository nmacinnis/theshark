-module(shark_irc_talk_server).

-behaviour(gen_server).

-export([start_link/0, say/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-import(shark_util, [env/1]).
-import(irc_basics, [process_code/1]).

-include("record.hrl").


%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    State = #state{},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

say(Text, Socket) ->
    gen_server:cast(?MODULE, {say, Text, Socket}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call(_, _From, State) ->
    {noreply, _From, State}.

handle_cast({say, Text, Socket}, State) ->
    io:format("sayin a thing ~s~n", [Text]),
    send(Socket, [irc_commands:say(env(irc_channel), Text)]),
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
