#!/usr/bin/env escript

main([FileName]) ->
    true = code:add_pathz("./ebin"),
    {ok, Config} = intcode:read_tape(FileName),
    {ok, Output} = intcode:run_program_until_end(Config, [2], []),
    io:format("Output: ~p~n", [Output]).
