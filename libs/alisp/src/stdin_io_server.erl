-module(stdin_io_server).
-export([start/0]).

start() ->
    {ok, Stdin} = atomvm:posix_open("/dev/stdin", [o_rdonly]),
    IOLeader = spawn(fun() -> io_loop(Stdin, <<>>) end),
    erlang:group_leader(IOLeader, self()),
    arepl:start().

io_loop(Stdin, Buf) ->
    receive
        {io_request, FPid, FRef, {get_line, unicode, Prompt}} ->
            console:print(Prompt),
            {Result, Rest} = read_line(Stdin, Buf),
            FPid ! {io_reply, FRef, Result},
            io_loop(Stdin, Rest);
        {io_request, FPid, FRef, {put_chars, unicode, Data}} ->
            console:print(Data),
            FPid ! {io_reply, FRef, ok},
            io_loop(Stdin, Buf);
        _Other ->
            io_loop(Stdin, Buf)
    end.

read_line(Stdin, Buf) ->
    case find_newline(Buf) of
        {ok, Pos} ->
            Line = binary:part(Buf, 0, Pos + 1),
            Rest = binary:part(Buf, Pos + 1, byte_size(Buf) - Pos - 1),
            {binary_to_list(Line), Rest};
        not_found ->
            case try_read(Stdin) of
                {ok, Data} ->
                    read_line(Stdin, <<Buf/binary, Data/binary>>);
                eof ->
                    case Buf of
                        <<>> -> {eof, <<>>};
                        _ -> {binary_to_list(Buf), <<>>}
                    end
            end
    end.

try_read(Stdin) ->
    case atomvm:posix_read(Stdin, 1024) of
        {ok, Data} -> {ok, Data};
        {error, eagain} ->
            receive after 10 -> ok end,
            try_read(Stdin);
        eof -> eof;
        {error, _} -> eof
    end.

find_newline(Bin) ->
    find_newline(Bin, 0, byte_size(Bin)).

find_newline(_Bin, Pos, Size) when Pos >= Size ->
    not_found;
find_newline(Bin, Pos, Size) ->
    case binary:at(Bin, Pos) of
        10 -> {ok, Pos};
        _ -> find_newline(Bin, Pos + 1, Size)
    end.
