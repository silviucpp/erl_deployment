-module(erl_deployment_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    erl_deployment_sup:start_link().

stop(_State) ->
    ok.
