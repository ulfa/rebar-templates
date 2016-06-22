-module({{appid}}_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD_ARG_3(I, Type, Arg1, Arg2, Arg3, Arg4), {Arg1 , {I, start_link, [Arg1, Arg2, Arg3, Arg4]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Service, Config, Messages) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Service, Config, Messages]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Service, Config, Messages]) ->
    case janga_config:get_value(ui, Config, true) of
        true -> {ok, { {one_for_one, 5, 10}, 
                [
                setup_web(),
                ?CHILD_ARG_3(service_sup, supervisor, Service ++ "_service_sup", Service, Config, Messages),
                ?CHILD_ARG_3({{appid}}_logic_sup, supervisor, Service ++ "_logic_sup", Service, Config, Messages)
                ]}};
        false -> {ok, { {one_for_one, 5, 10},                 
                [
                ?CHILD_ARG_3(service_sup, supervisor, Service ++ "_service_sup", Service, Config, Messages),
                ?CHILD_ARG_3({{appid}}_logic_sup, supervisor, Service ++ "_logic_sup", Service, Config, Messages)
                ]}}
    end.

setup_web() ->
	Ip = get_ip(),
    {ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, get_port()},                 
                 {dispatch, Dispatch},
                 {dispatch_group, {{appid}} },
                 {name, {{appid}} },
                 {error_handler, {{appid}}_webmachine_error_handler}                            
                 ],               
                 lager:info("Ip: ~p, Port: ~p", [Ip, get_port()]),    
    { {{appid}}_instance, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic}.

get_port() ->
	janga_config:get_port({{appid}}).

get_ip() ->
    case application:get_env(janga_core, {{appid}}_ip) of 
        undefined ->  "0.0.0.0";
        {ok, Ip} -> Ip
    end.