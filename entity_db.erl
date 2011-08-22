%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%%
%%% Description : 
%%% Created : 
%%% -------------------------------------------------------------------
-module({{appid}}_db).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("../include/wurfler.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_db/0]).
-export([create/1, update/1, delete/1, find_by_id/1]).

create_db() ->
	case mnesia:create_schema([node()]) of 
		{error, Reason} -> error_logger:info_msg(Reason),
						   false;
		_ -> application:start(mnesia),
			 mnesia:create_table({{entity}},[{disc_copies, [node()]}, {attributes, record_info(fields, {{entity}})}]).
	end.

create(Entity) ->
	mnesia:activity(transaction, fun() -> mnesia:write(Entity, write) end).

read(Id) ->
	mnesia:activity(transaction, fun() -> mnesia:read(Id) end).

update(Entity) ->
	ok.

delete(Id) ->
	mnesia:activity(transaction, fun() -> mnesia:delete({{entity}}, Id) end).