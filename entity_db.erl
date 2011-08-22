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
-module({{entity}}_db).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("../include/{{appid}}.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_db/0]).
-export([create/1, update/1, delete/1, find_by_id/1, find_free_id/1]).

create_db() ->
	case mnesia:create_schema([node()]) of 
		{error, Reason} -> error_logger:info_msg(Reason),
						   false;
		_ -> application:start(mnesia),
			 mnesia:create_table({{entity}},[{disc_copies, [node()]}, {attributes, record_info(fields, {{entity}})}]),
			 mnesia:wait_for_tables([{{entity}}], 100000),
			 application:stop(mnesia)
	end.

create(Entity) ->
	mnesia:activity(transaction, fun() -> mnesia:write(Entity, write) end).

find_by_id(Id) ->
	mnesia:activity(transaction, fun() -> mnesia:read(Id) end).

update(Entity) ->
	ok.

delete(Id) ->
	mnesia:activity(transaction, fun() -> mnesia:delete({{entity}}, Id) end).

find_free_id(Id) ->
	case find_by_id(Id) of
		[] -> Id;
		_ -> find_free_id(Id, 0)
	end.

find_free_id(Id, Count) ->
	case find_by_id(Id ++ integer_to_list(Count)) of
		[] -> Id ++ integer_to_list(Count);
		_ -> find_free_id(Id, Count + 1)
	end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.