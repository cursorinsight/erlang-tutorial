-module(id_server_2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% @doc Start the supervisor.
-spec start_link() -> {ok, pid()} |
                      ignore |
                      {error, {already_started, pid()} |
                              {shutdown, term()} |
                              term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, % Name
                          ?MODULE,          % Callback module
                          #{}).             % Config

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%% @doc Return the supervisor's initial configuration.
-spec init(Config :: #{}) ->
          {ok, {SupFlags :: {supervisor:strategy(),
                             MaxR :: non_neg_integer(),
                             MaxT :: pos_integer()},
                ChildSpecs :: [supervisor:child_spec()]}} |
          ignore |
          {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
                [supervisor:child_spec()]}}.
init(_Config = #{}) ->
    {ok, {
       % Global options
       {
        % Supervisor strategy: what to do with other children when a child
        % is restarted.
        % - one_for_one: leave them
        % - one_for_all: restart all of them
        % - rest_for_one: restart only the ones after the restarted child
        one_for_one,

        % If more than 5 restarts occur within 10 seconds, the supervisor
        % terminates all child processes and then itself.
        5, 10},

       % Child processes
       [
        child(id_server_2, start_link, [])
       ]
      }}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec child(Mod :: module(),
            Fun :: atom(),
            Args :: [term()]) -> supervisor:child_spec().
child(Mod, Fun, Args) ->
    % For Erlang/OTP >= 18.0:
    #{id => Mod,                  % Id of the child within the supervisor.

      start => {Mod, Fun, Args},  % How to start the child.

      restart => permanent,       % Should we restart the child?
                                  % - permanent = always
                                  % - transient = only if crashed
                                  % - temporary = never

      shutdown => ?TIMEOUT,       % Time to wait for children when terminating.

      type => worker,             % Child type: worker | supervisor.

      modules => [Mod]}.          % Modules used by the child process.

    % For any Erlang/OTP:
    %{Mod, {Mod, Fun, Args}, permanent, ?TIMEOUT, worker, [Mod]}.
