%% Copyright 2010 {{author}}
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
%%% Author  : {{author}} {{email}}
%%% Description :
%%%
%%% Created : {{date}}
%%% -------------------------------------------------------------------
-module({{appid}}_resource).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, content_types_provided/2, allowed_methods/2, resource_exists/2, finish_request/2]).
-export([options/2, allow_missing_post/2, post_is_create/2, create_path/2, last_modified/2]).
-export([content_types_accepted/2, generate_etag/2, delete_resource/2, delete_completed/2, is_conflict/2]).
-export([accept_content_json/2, accept_content_xml/2, provide_json_content/2, provide_xml_content/2]).
-export([is_authorized/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("../deps/webmachine/include/webmachine.hrl").
-include("../include/{{appid}}.hrl").
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {entity}).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
init(_Config) -> 
	%%{ok, #context{}}.
	{ {trace, "/tmp"}, #context{} }.
%
% Returning non-true values will result in 404 Not Found.
% 
resource_exists(ReqData, Context) ->
	case {{appid}}_db:find_by_id(wrq:disp_path(ReqData)) of 
		[] -> {false, ReqData, Context};
		[Entity] -> {true, ReqData, Context#context{entity = Entity}}
	end.
%
% true, if the service is available
%
service_available(ReqData, Context) ->
	{true, ReqData, Context}.

%
% If this returns anything other than true, the response will be 401 Unauthorized.
% The AuthHead return value will be used as the value in the WWW-Authenticate header.
%
is_authorized(ReqData, Context) ->
	{true, ReqData, Context}.

forbidden(ReqData, Context) ->
	{false, ReqData, Context}.

%
% If the resource accepts POST requests to nonexistent resources, 
% then this should return true.
%
allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

malformed_request(ReqData, Context) ->
	{false, ReqData, Context}.

uri_too_long(ReqData, Context) ->
	{false, ReqData, Context}.

known_content_type(ReqData, Context) ->
	{true, ReqData, Context}.

valid_entity_length(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If the OPTIONS method is supported and is used, the return value of this 
% function is expected to be a list of pairs representing header names and 
% values that should appear in the response.
%
options(ReqData, Context) ->
	{['GET', 'POST', 'DELETE', 'PUT'], ReqData, Context}.

valid_content_headers(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If a Method not in this list is requested, then a 405 Method Not Allowed
% will be sent. Note that these are all-caps and are atoms. (single-quoted)
%
allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'DELETE', 'PUT'], ReqData, Context}.
%
% This is called when a DELETE request should be enacted 
% and should return true if the deletion succeeded.
%
delete_resource(ReqData, Context) ->
	case delete(wrq:disp_path(ReqData)) of
		ok -> {true, ReqData, Context};
		_ -> {false, ReqData, Context}
	end.
%
% This is only called after a successful delete_resource call, and should 
% return false if the deletion was accepted but cannot yet be guaranteed to have finished.
%
delete_completed(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If POST requests should be treated as a request to put content into a 
% (potentially new) resource as opposed to being a generic submission for 
% processing, then this function should return true. If it does return true, 
% then create_path will be called and the rest of the request will be treated 
% much like a PUT to the Path entry returned by that call.
post_is_create(ReqData, Context) ->
	{true, ReqData, Context}.
%
% This will be called on a POST request if post_is_create returns true. 
% It is an error for this function to not produce a Path if post_is_create returns true. 
% The Path returned should be a valid URI part following the dispatcher prefix. 
% That Path will replace the previous one in the return value of wrq:disp_path(ReqData) 
% for all subsequent resource function calls in the course of this request.
%
create_path(ReqData, Context) ->		
	case wrq:get_req_header("slug", ReqData) of
        undefined -> {erlang:integer_to_list(euuid:random()), ReqData, Context};
        Slug ->
            case {{appid}}_db:find_by_id(Slug) of
				[] -> {Slug, ReqData, Context};
                _  -> { {{appid}}_db:find_free_id(Slug), ReqData, Context}
            end
    	end.
%
% If post_is_create returns false, then this will be called to process any POST requests. 
% If it succeeds, it should return true.
%
process_post(ReqData, Context) ->
	{false, ReqData, Context}.
%
% This should return a list of pairs where each pair is of the form {Mediatype, Handler} 
% where Mediatype is a string of content-type format and the Handler is an atom naming 
% the function which can provide a resource representation in that media type. Content 
% negotiation is driven by this return value. For example, if a client request includes 
% an Accept header with a value that does not appear as a first element in any of the 
% return tuples, then a 406 Not Acceptable will be sent.
% 
content_types_provided(ReqData, Context) ->
   %% {[{"application/json", provide_json_content}, {"application/xml", provide_xml_content}],ReqData, Context}.
	 {[{"application/json", provide_json_content}],ReqData, Context}.
%
% This is used similarly to content_types_provided, except that it is for incoming 
% resource representations -- for example, PUT requests. Handler functions usually 
% want to use wrq:req_body(ReqData) to access the incoming request body.
% 
content_types_accepted(ReqData, Context) ->
	{[{"application/json", accept_content_json}, {"application/xml", accept_content_xml}], ReqData, Context}.
%
% If this is anything other than the atom no_charset, it must be a list of pairs where 
% each pair is of the form Charset, Converter where Charset is a string naming a charset
% and Converter is a callable function in the resource which will be called on the produced 
% body in a GET and ensure that it is in Charset.
%
charsets_provided(ReqData, Context) ->
	{no_charset, ReqData, Context}.
%
% This must be a list of pairs where in each pair Encoding is a string naming a valid 
% content encoding and Encoder is a callable function in the resource which will be called 
% on the produced body in a GET and ensure that it is so encoded. One useful setting is to 
% have the function check on method, and on GET requests return 
% [{"identity", fun(X) -> X end}, {"gzip", fun(X) -> zlib:gzip(X) end}] as this is all that 
% is needed to support gzip content encoding.
%
encodings_provided(ReqData, Context) ->
	{[{"identity", fun(X) -> X end}], ReqData, Context}.
%
% If this function is implemented, it should return a list of strings with header names that 
% should be included in a given response's Vary header. The standard conneg headers 
% (Accept, Accept-Encoding, Accept-Charset, Accept-Language) do not need to be specified 
% here as Webmachine will add the correct elements of those automatically depending on resource behavior.
%
variances(ReqData, Context) ->
	{[], ReqData, Context}.
%
% If this returns true, the client will receive a 409 Conflict.
%
is_conflict(ReqData, Context) ->
	{false, ReqData, Context}.
%
% If this returns true, then it is assumed that multiple representations of the response are possible 
% and a single one cannot be automatically chosen, so a 300 Multiple Choices will be sent instead of a 200.
% 
multiple_choices(ReqData, Context) ->
	{false, ReqData, Context}.

previously_existed(ReqData, Context) ->
	{false, ReqData, Context}.
%
% {true, MovedURI} | false
%
moved_permanently(ReqData, Context) ->
	{false, ReqData, Context}.
%
% {true, MovedURI} | false
%
moved_temporarily(ReqData, Context) ->
	{false, ReqData, Context}.
%
% undefined | YYYY,MM,DD, Hour,Min,Sec
%
last_modified(ReqData,#context{entity = Entity} = Context) ->
	{Entity#{{entity}}.lastmodified, ReqData, Context}.
%
% undefined | YYYY,MM,DD, Hour,Min,Sec 
%
expires(ReqData, Context) ->
	{undefined, ReqData, Context}.
%
% If this returns a value, it will be used as the value of the ETag header 
% and for comparison in conditional requests.
%
generate_etag(ReqData,  #context{entity = Entity} = Context) ->
	{mochihex:to_hex(erlang:phash2(Entity#{{entity}}.data)), ReqData, Context}.
% This function, if exported, is called just before the final response is constructed and sent. 
% The Result is ignored, so any effect of this function must be by returning a modified ReqData 
%
finish_request(ReqData, Context) ->
	{true, ReqData, Context}.
%% --------------------------------------------------------------------
%%% Additional functions
%% --------------------------------------------------------------------
accept_content_json(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	{struct, Properties} = mochijson2:decode(Body),	
	E = [{"id", wrq:disp_path(ReqData)}|[convert({K,V}) || {K,V} <- Properties]],
	case Context#context.entity of
		undefined -> create(json, E);
		Entity -> Entity#{{entity}}{data = E},
				  update(Entity)
	end,
	{true, wrq:set_resp_body(to_json(E), ReqData), Context}.

accept_content_xml(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	{true, ReqData, Context}.

provide_json_content(ReqData, Context) ->
	case {{appid}}_db:find_by_id(wrq:disp_path(ReqData)) of
		[] -> {error, ReqData, Context};
		[Entity] -> {to_json(Entity#{{entity}}.data), ReqData, Context}
	end.

provide_xml_content(ReqData, Context) ->
	case {{appid}}_db:find_by_id(wrq:disp_path(ReqData)) of
		[] -> {error, ReqData, Context};
		[Entity] -> {to_xml(Entity#{{entity}}.data), ReqData, Context}
	end.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
create(json, Properties) ->
	{{appid}}_db:create(json, Properties);
create(xml, Entity) ->
	{{appid}}_db:create(xml, Entity).
update(Entity) ->
	{{appid}}_db:update(Entity).
delete(Id) ->
	{{appid}}_db:delete(Id).
find_by_id(Id) ->
	{{appid}}_db:find_by_id(Id).

convert({Key, Value}) when is_list(Key), is_list(Value) ->
	{list_to_atom(Key), list_to_binary(Value)};
convert({Key, Value}) when is_binary(Key), is_binary(Value) ->
	{binary_to_list(Key), binary_to_list(Value)}.
convert(xml, {Key, Value}) ->
	{list_to_atom(Key), [Value]}.
to_json(Body) ->
	mochijson:encode({struct,[convert({K, V}) || {K,V} <- Body]}).
to_xml(Body) ->
	A = { {{entity}}, [convert(xml, {K, V}) || {K, V} <- Body]},
	Xml = lists:flatten(xmerl:export_simple_content([A], xmerl_xml)),
	iolist_to_binary([<<X/utf8>> || X <- Xml]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
encode_test() ->
	A = [{"forename", "ulf"}, {"surename", "angermann"}],
	B = [convert({K, V}) || {K,V} <- A],
	?assertEqual(<<"{\"forename\":\"ulf\",\"surename\":\"angermann\"}">>, erlang:iolist_to_binary(mochijson:encode({struct, B}))).

to_xml_test() ->
	A = [{"forename", "ulf"}, {"surename", "angermann"}],
	B = [convert(xml, {K, V}) || {K, V} <- A],
	lists:flatten(xmerl:export_simple_content([{user, B}], xmerl_xml)).

-endif.