#!/usr/bin/env escript

num_op(Tape, Position, Op) ->
    A = array:get(array:get(Position+1, Tape), Tape),
    B = array:get(array:get(Position+2, Tape), Tape),
    WritePosition = array:get(Position+3, Tape),
    NewValue = Op(A, B),
    NewTape = array:set(WritePosition, NewValue, Tape),
    {ok, NewTape, Position + 4}.

add(Tape, Position) ->
    num_op(Tape, Position, fun(A,B) -> A+B end).

mult(Tape, Position) ->
    num_op(Tape, Position, fun(A,B) -> A*B end).

stop(_, _) -> stop.

run_program(Tape, Position, OpTable) ->
    Op = maps:get(array:get(Position, Tape), OpTable),
    case Op(Tape, Position) of
        {ok, NewTape, NewPosition} -> run_program(NewTape, NewPosition, OpTable);
        stop -> Tape
    end.

main([]) ->
    OpTable = #{
        1  => fun(Tape, Position) -> add(Tape,  Position) end,
        2  => fun(Tape, Position) -> mult(Tape, Position) end,
        99 => fun(Tape, Position) -> stop(Tape, Position) end
    },
    Line = io:get_line(""),
    TrimmedLine = string:trim(Line, both, "\n"),
    RawTape = string:split(TrimmedLine, ",", all),
    Tape = array:from_list([list_to_integer(X) || X <- RawTape]),
    lists:foreach( fun( Noun ) ->
        lists:foreach( fun( Verb ) ->
            InputTape = array:set(1, Noun, array:set(2, Verb, Tape) ),
            io:format("~B ~B ~B~n", [Noun, Verb, array:get(0, run_program( InputTape, 0, OpTable ))])
        end, lists:seq(0,99) )
    end, lists:seq(0, 99) ),
    NewTape = run_program(Tape, 0, OpTable),
    io:format("~B~n", [array:get(0,NewTape)]).
