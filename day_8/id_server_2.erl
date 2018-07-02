-module(id_server_2).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_id/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(TIMEOUT, 5000). % 5 seconds

-type state() :: #{next_id := integer()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE},       % Name
                          ?MODULE,                % Callback module
                          #{},                    % Config
                          [{timeout, ?TIMEOUT}]). % Options

-spec get_id() -> {ok, integer()}.
get_id() ->
    gen_server:call(?MODULE, get_id, ?TIMEOUT).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%% @doc Initialize the server.
-spec init(Config :: #{}) ->
          {ok, state()} |
          {ok, state(), timeout()} |
          {stop, Reason :: term()} |
          ignore.
init(_Config = #{}) ->
    {ok, _State = #{next_id => 0}}.

%% @doc Handle synchronous requests.
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
          {reply, Reply :: term(), state()} |
          {reply, Reply :: term(), state(), timeout()} |
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), Reply :: term(), state()} |
          {stop, Reason :: term(), state()}.
handle_call(get_id, _From, #{next_id := NextId} = State) ->
    {reply, {ok, NextId}, State#{next_id := NextId + 1}};
handle_call(_Request, _From, State) ->
    % TODO: Log a warning.
    {reply, ok, State}.

%% @doc Handle asynchronous requests.
-spec handle_cast(Msg :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_cast(_Request, State) ->
    % TODO: Log a warning.
    {noreply, State}.

%% @doc Handle any other message.
-spec handle_info(Info :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_info(_Info, State) ->
    % TODO: Log a warning.
    {noreply, State}.

%% @doc Clean up the gen_server state.
-spec terminate(Reason :: term(),
                State :: state()) -> any().
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed.
-spec code_change(OldVsn :: term() | {down, Vsn :: term()},
                  State :: state(),
                  Extra :: term()) ->
          {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
