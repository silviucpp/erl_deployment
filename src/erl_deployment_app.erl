-module(erl_deployment_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    P = erl_deployment_sup:start_link(),
    erl_systemd:wait_for_pid(5000),
    P.

stop(_State) ->
    ok.
