-module(shark_memer_server).

-behaviour(gen_server).

-export([start_link/0, post/1, update_socket/1]).
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
    gen_server:call(?MODULE, {status, Status}).

update_socket(Socket) ->
    gen_server:call(?MODULE, {update_socket, Socket}).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(State) ->
    {ok, State}.

handle_call({update_socket, Socket}, _From, State) ->
    io:format("updated socket is now ~w~n", [Socket]),
    UpdatedState = State#state{socket = Socket},
    {reply, _From, UpdatedState};
handle_call({status, Status}, _From, State) ->
    case post_status(Status) of
        nothing ->
            {noreply, _From, State};
        dang ->
            {noreply, _From, State};
        Url ->
            {reply, Url, State}
    end.

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

post_status(Status) ->
    SplitList = re:split(Status, "\\|", [{return, list}]),
    case SplitList of
        [Status1, Status2 | _ ] ->
            T1 = "&t1=" ++ http_uri:encode(Status1),
            T2 = "&t2=" ++ http_uri:encode(Status2),
            MemeCaptainUrl    = env(meme_captain_url),
            Url = MemeCaptainUrl ++ T1 ++ T2,
            Response = httpc:request(Url),
            case Response of
                {ok, _} ->
                    Url;
                {error, _} ->
                    dang
            end;
        _ ->
            nothing
    end.

