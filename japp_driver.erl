%%
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
-module({{appid}}_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../deps/janga_core/include/janga_core.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------

-export([janga_handle_msg/3, janga_invoke/2, janga_init/1, janga_terminate/1]).

janga_init(Config) ->
	lager:info("~p:janga_init('~p')", [?MODULE, Config]),
    {ok, Config}.

janga_terminate(Config) ->
	lager:info("~p:janga_terminate('~p')", [?MODULE, Config]),
    {ok, Config}.

janga_handle_msg([Node ,Module, Id, Time, Optional, Payload], Config, Module_config) ->
	Config.

janga_invoke(Config, Module_config) ->
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
