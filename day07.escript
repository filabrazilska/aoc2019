#!/usr/bin/env escript

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

inner_amplify([{Config, Input, OutputSoFar} | RestOfMachines]) ->
    case intcode:run_program(Config, Input, OutputSoFar) of
        {stop, Output} ->
            {ok, Output};
        {input, NewConfig, NewInput, NewOutput} ->
            inner_amplify(RestOfMachines ++ [{NewConfig, NewInput, NewOutput}]); % for 5 machines the '++' operation is OK
        {output, NewConfig, NewInput, [OutputValue | NewOutput]} ->
            [{NextConfig, NextInput, NextOutput} | RestRestOfMachines] = RestOfMachines,
            inner_amplify([{NextConfig, NextInput ++ [OutputValue], NextOutput} | RestRestOfMachines] ++ [{NewConfig, NewInput, NewOutput}])
    end. 

amplify(Config, [First | AmplifierSetup]) ->
    InitialConfigs = [ {intcode:copy_config(Config), [First, 0], []} | [{intcode:copy_config(Config), [I], []} || I <- AmplifierSetup] ],
    {ok, Output} = inner_amplify(InitialConfigs), 
    io:format(">>>~p~n", [Output]).

main([FileName]) ->
    true = code:add_pathz("./ebin"),
    {ok, Config} = intcode:read_tape(FileName),
    Perms = perms(lists:seq(5,9)),
    lists:foreach(fun(P) -> amplify(Config, P) end, Perms),
    ok.
