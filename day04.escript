#!/usr/bin/env escript

is_valid(N) ->
    [[Fst|Rest]] = io_lib:format("~B", [N]),
    {_, LastGroupLen, PairExists, AllNonDeclining} = lists:foldl(
      fun(Char, {Last, GroupLen, SawPair, NonDeclining}) ->
            case Char of
                C when C =:= Last ->
                    {C, GroupLen+1, SawPair, NonDeclining};
                D when D < Last ->
                    {D, 1, SawPair, false};
                E ->
                    {E, 1, (SawPair or (GroupLen =:= 2)), NonDeclining}
            end
      end, {Fst, 1, false, true}, Rest),
    (PairExists or (LastGroupLen =:= 2)) and AllNonDeclining.

how_many(Low, High) ->
    length([X || X <- lists:seq(Low, High), is_valid(X) =:= true]).

main([]) ->
    Low = 278384,
    High = 824795,
    io:format("~B~n", [how_many( Low, High )]).
