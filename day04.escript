#!/usr/bin/env escript

is_valid(N) ->
    case N rem 10000 of
        0 -> io:format("=== ~B~n", [N]);
        _ -> noop
    end,
    [[Fst|Rest]] = io_lib:format("~B", [N]),
    {_, PairExists, AllNonDeclining} = lists:foldr(
      fun(Char, {Last, SawPair, NonDeclining}) ->
            case Char of
                C when C =:= Last ->
                    {C, true, NonDeclining};
                D when D < Last ->
                    {D, SawPair, false};
                E ->
                    {E, SawPair, NonDeclining}
            end
      end, {Fst, false, true}, Rest),
    PairExists and AllNonDeclining.

how_many(Low, High) ->
    length([X || X <- lists:seq(Low, High), is_valid(X) =:= true]).

main([]) ->
    Low = 278384,
    High = 824795,
    io:format("~B~n", [how_many( 111109, 111112 )]),
    io:format("~B~n", [how_many( Low, High )]).
