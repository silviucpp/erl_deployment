%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1]).
-export([reload_all_changed/0]).

reload_all_changed() ->
    reload_modules(all_changed()).

%% @spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
    lists:foreach(fun(M) ->
        code:purge(M),
        case code:load_file(M) of
            {error, Reason} -> io:format("Failed to reload: ~s - ~s~n", [M, Reason]);
            _ -> io:format("Reloaded: ~s~n", [M])
        end
    end, Modules).

%% @spec all_changed() -> [atom()]
%% @doc Return a list of beam modules that have changed.
all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    Attrs = proplists:get_value(attributes, L),
    proplists:get_value(vsn, Attrs).