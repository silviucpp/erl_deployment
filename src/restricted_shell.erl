-module(restricted_shell).

-export([
    local_allowed/3,
    non_local_allowed/3
]).

local_allowed(q, [], _State) ->
    {false, _State};
local_allowed({init, stop}, [], _State) ->
    {false, _State};
local_allowed({erlang, halt}, [], _State) ->
    {false, _State};
local_allowed(_Func, _ArgList, _State) ->
    {true, _State}.

non_local_allowed(q, [], _State) ->
    {false, _State};
non_local_allowed({init, stop}, [], _State) ->
    {false, _State};
non_local_allowed({erlang, halt}, [], _State) ->
    {false, _State};
non_local_allowed(_Func, _ArgList, _State) ->
    {true, _State}.
