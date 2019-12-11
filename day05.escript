#!/usr/bin/env escript

%% dump_tape_diff(OldTape, NewTape) ->
%%     DifferentIndices = lists:foldl(
%%                          fun(I, Acc) ->
%%                                  case array:get(I, OldTape) =:= array:get(I, NewTape) of
%%                                      true -> Acc;
%%                                      false -> [I | Acc]
%%                                  end
%%                          end,
%%                          [], lists:seq(0, array:size(OldTape)-1)),
%%     case DifferentIndices of
%%         [] -> ok;
%%         _ ->
%%             io:format("[~s]~n", [lists:join(",",
%%                                           lists:map(fun (I) ->
%%                                                             io_lib:format("[~B] (~B -> ~B)",
%%                                                                           [I, array:get(I, OldTape), array:get(I, NewTape)])
%%                                                     end, DifferentIndices
%%                                                    )
%%                                          )
%%                               ]
%%                      )
%%     end.

get_param(Tape, Position, Offset, OpParams) ->
    Mode = case Offset of
        1 -> OpParams rem 10;
        2 -> OpParams div 10
    end,
    case Mode of
        0 ->
            Value = array:get(array:get(Position+Offset, Tape), Tape),
            %% io:format("P(~B->~B)", [array:get(Position+Offset, Tape),Value]),
            Value;
        1 ->
            Value = array:get(Position+Offset, Tape),
            %% io:format("I(~B)", [Value]),
            Value
    end.

num_op(Tape, Position, Op, OpParams) ->
    %% io:format("(~B,~B,~B)",
              %% [array:get(Position+1, Tape),
               %% array:get(Position+2, Tape),
               %% array:get(Position+3, Tape)]),
    %% io:format(", A="),
    A = get_param(Tape, Position, 1, OpParams),
    %% io:format(", B="),
    B = get_param(Tape, Position, 2, OpParams),
    WritePosition = array:get(Position+3, Tape),
    NewValue = Op(A, B),
    %% io:format(", WritePosition: ~B, Value: ~B -> ~B", [WritePosition, array:get(WritePosition, Tape), NewValue]),
    NewTape = array:set(WritePosition, NewValue, Tape),
    %% io:nl(),
    {ok, NewTape, Position + 4}.

add(Tape, Position, OpParams) ->
    num_op(Tape, Position, fun(A,B) -> A+B end, OpParams).

mult(Tape, Position, OpParams) ->
    num_op(Tape, Position, fun(A,B) -> A*B end, OpParams).

stop(_, _, _) -> stop.

input(Tape, Position, _OpParams) ->
    {ok, [InputValue]} = io:fread("", "~d"),
    WritePosition = array:get(Position+1, Tape),
    %% io:format("(~B), A=I(~B), WritePosition: ~B, InputValue: ~B~n", [array:get(Position+1, Tape), WritePosition, WritePosition, InputValue]),
    NewTape = array:set(WritePosition, InputValue, Tape),
    %% io:nl(),
    {ok, NewTape, Position + 2}.

output(Tape, Position, OpParams) ->
    Value = get_param(Tape, Position, 1, OpParams),
    %% io:nl(),
    io:format("~B~n", [Value]),
    {ok, Tape, Position + 2}.

format_opcode(1) -> "+";
format_opcode(2) -> "*";
format_opcode(3) -> ">>";
format_opcode(4) -> "<<";
format_opcode(99) -> ";";
format_opcode(_) -> "?".

run_program(Tape, Position, OpTable) ->
    FullOp = array:get(Position, Tape),
    OpCode = FullOp rem 100,
    OpParams = FullOp div 100,
    OpCodeString = format_opcode(OpCode),
    %% io:format("FullOp: ~B (~B), OpCode: ~s, OpParams: ~B~n", [FullOp, Position, OpCodeString, OpParams]),
    Op = maps:get(OpCode, OpTable),
    case Op(Tape, Position, OpParams) of
        {ok, NewTape, NewPosition} ->
            run_program(NewTape, NewPosition, OpTable);
        stop -> Tape
    end.

main([]) ->
    OpTable = #{
        1  => fun(Tape, Position, OpParams) -> add(Tape,    Position, OpParams) end,
        2  => fun(Tape, Position, OpParams) -> mult(Tape,   Position, OpParams) end,
        3  => fun(Tape, Position, OpParams) -> input(Tape,  Position, OpParams) end,
        4  => fun(Tape, Position, OpParams) -> output(Tape, Position, OpParams) end,
        99 => fun(Tape, Position, OpParams) -> stop(Tape,   Position, OpParams) end
    },
    Line = io:get_line(""),
    TrimmedLine = string:trim(Line, both, "\n"),
    RawTape = string:split(TrimmedLine, ",", all),
    Tape = array:from_list([list_to_integer(X) || X <- RawTape]),
    NewTape = run_program(Tape, 0, OpTable).
