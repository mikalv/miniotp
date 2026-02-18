-module(alisp_runtime).
-export([receive_any/1, receive_msg/2, send/2]).

%% Receive any message with timeout (ms)
receive_any(Timeout) ->
    receive
        Msg -> {ok, Msg}
    after Timeout ->
        timeout
    end.

%% Receive a message matching a specific pattern atom/value
receive_msg(Pattern, Timeout) ->
    receive
        Pattern -> {ok, Pattern}
    after Timeout ->
        timeout
    end.

%% Send a message to a pid (convenience wrapper)
send(Pid, Msg) ->
    Pid ! Msg.
