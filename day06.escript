#!/usr/bin/env escript

parse_line(Line) ->
    TrimmedLine = string:trim(Line),
    [From, To] = string:split(TrimmedLine, ")"),
    {From, To}.

read_lines_and_build_graph(Graph) ->
    case io:get_line("") of
        eof -> Graph;
        Line ->
            {From, To} = parse_line(Line),
            GraphFromUpdated = case maps:is_key(From, Graph) of
                true ->
                    Succs = [To | maps:get(From, Graph)],
                    maps:update(From, Succs, Graph);
                false ->
                    maps:put(From, [To], Graph)
            end,
            GraphToUpdated = case maps:is_key(To, Graph) of
                true -> GraphFromUpdated;
                false ->
                    maps:put(To, [], GraphFromUpdated)
            end,
            read_lines_and_build_graph(GraphToUpdated)
    end.

traverse_and_accumulate_lens(Graph, From, CurrentLen) ->
    case maps:get(From, Graph) of
        [] -> CurrentLen;
        Succs ->
            CurrentLen + lists:foldr(fun(N, Lens) -> Lens + traverse_and_accumulate_lens(Graph, N, CurrentLen+1) end,
                                     0,
                                     Succs)
    end.

main([]) ->
    Graph = read_lines_and_build_graph(#{}),
    TotalLen = traverse_and_accumulate_lens(Graph, "COM", 0),
    io:format("Total length: ~B~n", [TotalLen]).
