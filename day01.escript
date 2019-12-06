#!/usr/bin/env escript

accumulate_fuel(Acc, Weight) ->
    case Weight div 3 - 2 of
        W when W < 1 -> Acc;
        W2 -> accumulate_fuel(Acc+W2, W2)
    end.

process_line(Line) ->
    TrimmedLine = string:trim(Line, both, "\n"),
    Num = list_to_integer(TrimmedLine),
    accumulate_fuel(0, Num).

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
