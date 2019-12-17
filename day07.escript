#!/usr/bin/env escript

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

inner_amplify([{Tape, Position, Input, OutputSoFar} | RestOfMachines] = Params) ->
    case intcode:run_program(Tape, Position, Input, OutputSoFar) of
        {stop, Output} ->
            {ok, Output};
        {input, NewTape, NewPosition, NewInput, NewOutput} = Y ->
            inner_amplify(RestOfMachines ++ [{NewTape, NewPosition, NewInput, NewOutput}]); % for 5 machines the '++' operation is OK
        {output, NewTape, NewPosition, NewInput, [OutputValue | NewOutput]} = X ->
            [{NextTape, NextPosition, NextInput, NextOutput} | RestRestOfMachines] = RestOfMachines,
            inner_amplify([{NextTape, NextPosition, NextInput ++ [OutputValue], NextOutput} | RestRestOfMachines] ++ [{NewTape, NewPosition, NewInput, NewOutput}])
    end. 

amplify(Tape, [First | AmplifierSetup]) ->
    InitialConfigs = [ {Tape, 0, [First, 0], []} | [{Tape, 0, [I], []} || I <- AmplifierSetup] ],
    {ok, Output} = inner_amplify(InitialConfigs), 
    io:format(">>>~p~n", [Output]).

main([FileName]) ->
    true = code:add_pathz("./ebin"),
    {ok, IO} = file:open(FileName, [read]),
    Line = io:get_line(IO,""),
    TrimmedLine = string:trim(Line, both, "\n"),
    RawTape = string:split(TrimmedLine, ",", all),
    Tape = array:from_list([list_to_integer(X) || X <- RawTape]),
    Perms = perms(lists:seq(5,9)),
    lists:foreach(fun(P) -> amplify(Tape, P) end, Perms),
    ok.
