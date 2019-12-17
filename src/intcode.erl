-module(intcode).

-export([run_program/4]).

-define(FILL_IO(Res,I,O),
        case Res of
            stop -> {stop, O};
            _ ->
                {ok,NT,NP} = Res,
                {ok,NT,NP,I,O}
        end).

-spec run_program(array:array(integer()), non_neg_integer(), [integer()], [integer()]) -> {stop, [integer()]} |
                                                                                          {input, array:array(integer()), non_neg_integer(), [integer()], [integer()]} |
                                                                                          {output, array:array(integer()), non_neg_integer(), [integer()], [integer()]}.
run_program(Tape, Position, Input, Output) ->
    FullOp = array:get(Position, Tape),
    OpCode = FullOp rem 100,
    OpParams = FullOp div 100,
    %% OpCodeString = format_opcode(OpCode),
    %% io:format("FullOp: ~B (~B), OpCode: ~s, OpParams: ~B~n", [FullOp, Position, OpCodeString, OpParams]),
    Op = maps:get(OpCode, optable()),
    case Op(Tape, Position, OpParams, Input, Output) of
        {ok, NewTape, NewPosition, NewInput, NewOutput} ->
            run_program(NewTape, NewPosition, NewInput, NewOutput);
        {stop, Out} -> {stop, lists:reverse(Out)};
        {input, _NewTape, _NewPosition, _NewInput, _NewOutput} = I -> I;
        {output, _NewTape, _NewPosition, _NewInput, _NewOutput} = O -> O
    end.

optable() ->
    #{
        1  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(add(Tape,    Position, OpParams), I, O) end,
        2  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(mult(Tape,   Position, OpParams), I, O) end,
        3  => fun(Tape, Position, OpParams, Input, O) ->
                      case input(Tape,  Position, OpParams, Input) of
                          {ok,NT,NP,NI} -> {ok, NT, NP, NI, O};
                          {input, NT, NP} -> {input, NT, NP, [], O}
                      end
              end,
        4  => fun(Tape, Position, OpParams, I, Output) ->
                  {output,NT,NP,NO} = output(Tape, Position, OpParams, Output),
                  {output,NT,NP,I,NO}
              end,
        5  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(jtrue(Tape,  Position, OpParams), I, O) end,
        6  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(jfalse(Tape, Position, OpParams), I, O) end,
        7  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(less(Tape,   Position, OpParams), I, O) end,
        8  => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(equals(Tape, Position, OpParams), I, O) end,
        99 => fun(Tape, Position, OpParams, I, O) -> ?FILL_IO(stop(Tape,   Position, OpParams), I, O) end
    }.

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

jtrue(Tape, Position, OpParams) ->
    Value = get_param(Tape, Position, 1, OpParams),
    NewPosition = get_param(Tape, Position, 2, OpParams),
    case Value of
        0 -> {ok, Tape, Position + 3};
        _ -> {ok, Tape, NewPosition}
    end.

jfalse(Tape, Position, OpParams) ->
    Value = get_param(Tape, Position, 1, OpParams),
    NewPosition = get_param(Tape, Position, 2, OpParams),
    case Value of
        0 -> {ok, Tape, NewPosition};
        _ -> {ok, Tape, Position + 3}
    end.

less(Tape, Position, OpParams) ->
    A = get_param(Tape, Position, 1, OpParams),
    B = get_param(Tape, Position, 2, OpParams),
    WritePosition = array:get(Position+3, Tape),
    ToStore = case A<B of
                  true -> 1;
                  _ -> 0
              end,
    NewTape = array:set(WritePosition, ToStore, Tape),
    {ok, NewTape, Position+4}.

equals(Tape, Position, OpParams) ->
    A = get_param(Tape, Position, 1, OpParams),
    B = get_param(Tape, Position, 2, OpParams),
    WritePosition = array:get(Position+3, Tape),
    ToStore = case A =:= B of
                  true -> 1;
                  _ -> 0
              end,
    NewTape = array:set(WritePosition, ToStore, Tape),
    {ok, NewTape, Position+4}.

stop(_, _, _) -> stop.

input(Tape, Position, _OpParams, []) -> {input, Tape, Position}; %% Yield if there is no input ready
input(Tape, Position, _OpParams, [InputValue | NewInput]) ->
    WritePosition = array:get(Position+1, Tape),
    %% io:format("(~B), A=I(~B), WritePosition: ~B, InputValue: ~B~n", [array:get(Position+1, Tape), WritePosition, WritePosition, InputValue]),
    NewTape = array:set(WritePosition, InputValue, Tape),
    %% io:nl(),
    {ok, NewTape, Position + 2, NewInput}.

output(Tape, Position, OpParams, Output) ->
    Value = get_param(Tape, Position, 1, OpParams),
    %% io:nl(),
    io:format("~B~n", [Value]),
    {output, Tape, Position + 2, [Value | Output]}.

format_opcode(1) -> "+";
format_opcode(2) -> "*";
format_opcode(3) -> ">>";
format_opcode(4) -> "<<";
format_opcode(5) -> "je";
format_opcode(6) -> "jne";
format_opcode(7) -> "le";
format_opcode(8) -> "eq";
format_opcode(99) -> ";";
format_opcode(_) -> "?".

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

