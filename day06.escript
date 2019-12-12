#!/usr/bin/env escript

parse_line(Line) ->
    TrimmedLine = string:trim(Line),
    [From, To] = string:split(TrimmedLine, ")"),
    {From, To}.

read_lines_and_build_graph(Graph, MyOrbit, SantaOrbit) ->
    case io:get_line("") of
        eof -> {Graph, MyOrbit, SantaOrbit};
        Line ->
            {From, To} = parse_line(Line),
            ToAdd = case To of
                "SAN" -> [];
                "YOU" -> [];
                _ -> [To]
            end,
            GraphFromUpdated = case maps:is_key(From, Graph) of
                true ->
                    {FromsFrom, Succs} = maps:get(From, Graph),
                    maps:update(From, {FromsFrom, ToAdd ++ Succs}, Graph);
                false ->
                    maps:put(From, {none, ToAdd}, Graph)
            end,
            case To of
                "SAN" ->
                    read_lines_and_build_graph(GraphFromUpdated, MyOrbit, From);
                "YOU" ->
                    read_lines_and_build_graph(GraphFromUpdated, From, SantaOrbit);
                _ ->
                    GraphToUpdated = case maps:is_key(To, GraphFromUpdated) of
                        true ->
                            {_, ToSuccs} = maps:get(To, GraphFromUpdated),
                            maps:update(To, {From, ToSuccs}, GraphFromUpdated);
                        false ->
                            maps:put(To, {From, []}, GraphFromUpdated)
                    end,
                    read_lines_and_build_graph(GraphToUpdated, MyOrbit, SantaOrbit)
            end
    end.

inner_traverse_and_count_jumps(Graph, TargetOrbit, VisitedOrbits, Queue) ->
    {{value, {CurrentJumps,CurrentOrbit}}, QueueWithoutCurrent} = queue:out(Queue),
    case CurrentOrbit =:= TargetOrbit of
        true ->
            CurrentJumps;
        false ->
            {OrbitPred, OrbitSuccs} = maps:get(CurrentOrbit, Graph),
            ToVisit = lists:filter(fun(I) -> ((I =/= none) and (not maps:is_key(I, VisitedOrbits))) end, [OrbitPred | OrbitSuccs]),
            NewQueue = lists:foldr(fun(TV, Q) -> queue:in({CurrentJumps+1,TV}, Q) end, QueueWithoutCurrent, ToVisit),
            NewVisitedOrbits = VisitedOrbits#{CurrentOrbit => 1},
            inner_traverse_and_count_jumps(Graph, TargetOrbit, NewVisitedOrbits, NewQueue)
    end.

traverse_and_count_jumps(Graph, CurrentOrbit, TargetOrbit) ->
    InitQueue = queue:new(),
    Queue = queue:in({0,CurrentOrbit}, InitQueue),
    inner_traverse_and_count_jumps(Graph, TargetOrbit, #{}, Queue).

main([]) ->
    {Graph, MyOrbit, SantaOrbit} = read_lines_and_build_graph(#{}, none, none),
    TotalLen = traverse_and_count_jumps(Graph, MyOrbit, SantaOrbit),
    io:format("Total length: ~B~n", [TotalLen]).
