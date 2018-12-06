-module(erl_systemd).

-define(WATCHDOG_DELAY, 60000).

-export([
    start_link/0,
    reloading/0,
    stopping/0,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    udp_socket,
    socket_path
}).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

reloading() ->
    safe_call(?MODULE, reloading).

stopping() ->
    safe_call(?MODULE, stopping).

init([]) ->
    case os:getenv("ERLANG_PID_FILE_PATH") of
        false ->
            ok;
        Path ->
            write_pid(Path)
    end,

    case open_socket() of
        {ok, Socket, SocketPath} ->
            erlang:send_after(0, self(), ready),
            erlang:send_after(?WATCHDOG_DELAY, self(), watchdog),
            {ok, #state{udp_socket = Socket, socket_path = SocketPath}};
        _ ->
            {ok, #state{}}
    end.

handle_call(reloading, _From, State) ->
    {reply, send_reloading(State), State};
handle_call(stopping, _From, State) ->
    {reply, send_stopping(State), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(ready, State) ->
    send_ready(State),
    {noreply, State};

handle_info(watchdog, State) ->
    send_watchdog(State),
    erlang:send_after(?WATCHDOG_DELAY, self(), watchdog),
    {noreply, State}.

terminate(_Reason, #state{udp_socket = Socket}) ->
    case Socket of
        undefined ->
            ok;
        _ ->

            gen_udp:close(Socket)
    end,
    ok.

% internals

write_pid(PidFile) ->
    {ok, F} = file:open(PidFile, [write, raw]),
    ok = file:write(F, os:getpid()),
    ok = file:close(F).

open_socket() ->
    case os:getenv("NOTIFY_SOCKET") of
        false ->
            undefined;
        Path ->
            case gen_udp:open(0, [local]) of
                {ok, Socket} ->
                    {ok, Socket, Path};
                Error ->
                    error_logger:warning_msg("open NOTIFY_SOCKET failed with: ~p", [Error]),
                    unedfined
            end
    end.

send_ready(#state{udp_socket = Socket, socket_path = Sp}) ->
    send_command(Socket, Sp, <<"READY=1">>).

send_watchdog(#state{udp_socket = Socket, socket_path = Sp}) ->
    send_command(Socket, Sp, <<"WATCHDOG=1">>).

send_stopping(#state{udp_socket = Socket, socket_path = Sp}) ->
    send_command(Socket, Sp, <<"STOPPING=1">>).

send_reloading(#state{udp_socket = Socket, socket_path = Sp}) ->
    send_command(Socket, Sp, <<"RELOADING=1">>).

send_command(Socket, SocketPath, Command) ->
    gen_udp:send(Socket, {local,SocketPath}, 0, Command).

safe_call(Receiver, Message) ->
    safe_call(Receiver, Message, 5000).

safe_call(Receiver, Message, Timeout) ->
    try
        gen_server:call(Receiver, Message, Timeout)
    catch
        exit:{noproc, _} ->
            {error, not_started};
        _: Exception ->
            {error, Exception}
    end.
