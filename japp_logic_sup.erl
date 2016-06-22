%% Copyright (c) 2015 Ulf Angermann  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module({{appid}}_logic_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).
-export([create_name/1]).

%% Helper macro for declaring children of supervisor
-define(SUPNAME(Name, Arg1), list_to_atom(lists:concat([Name, Arg1]))).
-define(CHILD_ARG_3(I, Type, Arg1, Arg2, Arg3), {Arg1 , {I, start_link, [Arg1, Arg2, Arg3]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================
%%
create_name(Service) ->
    ?SUPNAME(Service, "_logic_sup").

start_link(Name, Service, Config, Messages) ->
    supervisor:start_link({local, list_to_atom(Name)}, ?MODULE, [Service, Config, Messages]).

 % ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([_Service, _Config, _Messages]) ->
    RestartStrategy = {one_for_one, 3, 3600},
    {ok, {RestartStrategy, []}}.