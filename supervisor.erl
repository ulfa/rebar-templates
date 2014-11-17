-module({{id}}).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

-define(CHILD_1(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================
%%
start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 % ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, []}}.
