-module(erl_deployment_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
    ],
    {ok, {{one_for_one, 20, 1}, Children}}.
