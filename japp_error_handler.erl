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
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------

-module({{appid}}_webmachine_error_handler).

-export([render_error/3]).

render_error(Code, Req, Reason) ->
    case Req:has_response_body() of
        {true,_} ->
            maybe_log(Code, Req, Reason),
            Req:response_body();
        {false,_} -> render_error_body(Code, Req:trim_state(), Reason)
    end.

render_error_body(404, Req, _Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    %%lager:info("..... : ~p", [ReqState]),
    {ok, Content} = error_404_dtl:render([]),
    {Content, ReqState};

render_error_body(500, Req, Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    maybe_log(500, Req, Reason),
    STString = io_lib:format("~p", [Reason]),
    ErrorStart = "<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>",
    ErrorEnd = "</pre><P><HR><ADDRESS>mochiweb+webmachine web server</ADDRESS></body></html>",
    ErrorIOList = [ErrorStart,STString,ErrorEnd],
    {erlang:iolist_to_binary(ErrorIOList), ReqState};

render_error_body(501, Req, Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    {Method,_} = Req:method(),
    webmachine_log:log_error(501, Req, Reason),
    ErrorStr = io_lib:format("<html><head><title>501 Not Implemented</title>"
                             "</head><body><h1>Not Implemented</h1>"
                             "The server does not support the ~p method.<br>"
                             "<P><HR><ADDRESS>mochiweb+webmachine web server"
                             "</ADDRESS></body></html>",
                             [Method]),
    {erlang:iolist_to_binary(ErrorStr), ReqState};

render_error_body(503, Req, Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    webmachine_log:log_error(503, Req, Reason),
    ErrorStr = "<html><head><title>503 Service Unavailable</title>"
               "</head><body><h1>Service Unavailable</h1>"
               "The server is currently unable to handle "
               "the request due to a temporary overloading "
               "or maintenance of the server.<br>"
               "<P><HR><ADDRESS>mochiweb+webmachine web server"
               "</ADDRESS></body></html>",
    {list_to_binary(ErrorStr), ReqState};

render_error_body(Code, Req, Reason) ->
    {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
    ReasonPhrase = httpd_util:reason_phrase(Code),
    Body = ["<html><head><title>",
            integer_to_list(Code),
            " ",
            ReasonPhrase,
            "</title></head><body><h1>",
            ReasonPhrase,
            "</h1>",
            Reason,
            "<p><hr><address>mochiweb+webmachine web server</address></body></html>"],
    {iolist_to_binary(Body), ReqState}.

maybe_log(_Code, _Req, {error, {exit, normal, _Stack}}) ->
    %% webmachine_request did an exit(normal), so suppress this
    %% message. This usually happens when a chunked upload is
    %% interrupted by network failure.
    ok;
maybe_log(Code, Req, Reason) ->
    webmachine_log:log_error(Code, Req, Reason).
