#!/usr/bin/env escript

process_line(Line) ->
    TrimmedLine = string:trim(Line, both, "\n"),
    Num = list_to_integer(TrimmedLine),
    Num div 3 - 2.

try_next_line(Acc) ->
    case io:get_line("") of
        eof -> Acc;
        {error, Error} -> erlang:error(Error);
        Data ->
            try_next_line(Acc + process_line(Data))
    end.

main([]) ->
    try
        io:format("~B~n", [try_next_line(0)])
    catch
        E ->
            io:format("Wrong input (~s)~n", E),
            halt(1)
    end.
