-module(intcode).

-export([run_program/3, run_program_until_end/3]).
-export([copy_config/1, read_tape/1]).
-export_type([config/0]).

-define(FILL_IO(Res,I,O),
        case Res of
            stop -> {stop, O};
            _ ->
                {ok,NC} = Res,
                {ok,NC,I,O}
        end).

-record(config, {tape, position = 0, rel_base = 0}).
-opaque(config() :: #config{}).

-spec read_tape(string()) -> {ok, config()}.
read_tape(FileName) ->
    {ok, IO} = file:open(FileName, [read]),
    Line = io:get_line(IO,""),
    TrimmedLine = string:trim(Line, both, "\n"),
    RawTape = string:split(TrimmedLine, ",", all),
    Tape = array:from_list([list_to_integer(X) || X <- RawTape], 0),
    {ok, #config{tape = Tape, position = 0}}.

-spec copy_config(config()) -> config().
copy_config(C) ->
    Size = array:size(C#config.tape),
    NewTape0=array:new([{default,0},{size,Size},{fixed,false}]),
    NewTape=lists:foldl(fun(I,Acc) ->
                            array:set(I, array:get(I, C#config.tape), Acc)
                        end, NewTape0, lists:seq(0, Size-1)),
    #config{tape=NewTape, position=C#config.position, rel_base=C#config.rel_base}.

-spec run_program(#config{}, [integer()], [integer()]) -> {stop, [integer()]} |
                                                          {input, #config{}, [integer()], [integer()]} |
                                                          {output, #config{}, [integer()], [integer()]}.
run_program(Config, Input, Output) ->
    Tape = Config#config.tape,
    Position = Config#config.position,
    FullOp = array:get(Position, Tape),
    OpCode = FullOp rem 100,
    OpParams = FullOp div 100,
    OpCodeString = format_opcode(OpCode),
    io:format("FullOp: ~B (~B), OpCode: ~s, OpParams: ~p~n", [FullOp, Position, OpCodeString, OpParams]),
    Op = maps:get(OpCode, optable()),
    case Op(Config, OpParams, Input, Output) of
        {ok, NewConfig, NewInput, NewOutput} ->
            run_program(NewConfig, NewInput, NewOutput);
        {stop, Out} -> {stop, lists:reverse(Out)};
        {input, _NewConfig, _NewInput, _NewOutput} = I -> I;
        {output, _NewConfig, _NewInput, _NewOutput} = O -> O
    end.

-spec run_program_until_end(#config{}, [integer()], [integer()]) -> {ok, [integer()]}.
run_program_until_end(Config, Input, Output) ->
    case run_program(Config, Input, Output) of
        {ok, NewConfig, NewInput, NewOutput} ->
            run_program_until_end(NewConfig, NewInput, NewOutput);
        {stop, Out} -> {ok, lists:reverse(Out)};
        {input, NewConfig, NewInput, NewOutput} ->
            run_program_until_end(NewConfig, NewInput, NewOutput);
        {output, NewConfig, NewInput, NewOutput} ->
            run_program_until_end(NewConfig, NewInput, NewOutput)
    end.

optable() ->
    #{
        1  => fun(Config, OpParams, I, O) -> ?FILL_IO(add(Config, OpParams), I, O) end,
        2  => fun(Config, OpParams, I, O) -> ?FILL_IO(mult(Config, OpParams), I, O) end,
        3  => fun(Config, OpParams, Input, O) ->
                      case input(Config, OpParams, Input) of
                          {ok,NC,NI} -> {ok, NC, NI, O};
                          {input, NC} -> {input, NC, [], O}
                      end
              end,
        4  => fun(Config, OpParams, I, Output) ->
                  {output,NC,NO} = output(Config, OpParams, Output),
                  {output,NC,I,NO}
              end,
        5  => fun(Config, OpParams, I, O) -> ?FILL_IO(jtrue(Config, OpParams), I, O) end,
        6  => fun(Config, OpParams, I, O) -> ?FILL_IO(jfalse(Config, OpParams), I, O) end,
        7  => fun(Config, OpParams, I, O) -> ?FILL_IO(less(Config, OpParams), I, O) end,
        8  => fun(Config, OpParams, I, O) -> ?FILL_IO(equals(Config, OpParams), I, O) end,
        9  => fun(Config, OpParams, I, O) -> ?FILL_IO(adjust_rel_base(Config, OpParams), I, O) end,
        99 => fun(Config, OpParams, I, O) -> ?FILL_IO(stop(Config, OpParams), I, O) end
    }.

get_position(#config{tape = Tape, position = Position}, Offset, 0) -> array:get(Position + Offset, Tape);
get_position(#config{position = Position}, Offset, 1) -> Position + Offset;
get_position(#config{tape = Tape, position = Position, rel_base = RelBase}, Offset, 2) -> array:get(Position + Offset, Tape) + RelBase.

%% get_mode(OpParams, Offset)
get_mode(OpParams, 1) -> OpParams rem 10;
get_mode(OpParams, 2) -> (OpParams rem 100) div 10;
get_mode(OpParams, 3) -> OpParams div 100.

get_param(#config{tape = Tape} = Config, Offset, OpParams) ->
    array:get(get_position(Config, Offset, get_mode(OpParams, Offset)), Tape).

num_op(#config{tape = Tape, position = Position} = Config, Op, OpParams) ->
    %% io:format("(~B,~B,~B)",
              %% [array:get(Position+1, Tape),
               %% array:get(Position+2, Tape),
               %% array:get(Position+3, Tape)]),
    %% io:format(", A="),
    A = get_param(Config, 1, OpParams),
    %% io:format(", B="),
    B = get_param(Config, 2, OpParams),
    WritePosition = get_position(Config, 3, get_mode(OpParams, 3)),
    NewValue = Op(A, B),
    %% io:format(", WritePosition: ~B, Value: ~B -> ~B", [WritePosition, array:get(WritePosition, Tape), NewValue]),
    NewTape = array:set(WritePosition, NewValue, Tape),
    %% io:nl(),
    {ok, Config#config{tape = NewTape, position = Position + 4}}.

add(Config, OpParams) ->
    num_op(Config, fun(A,B) -> A+B end, OpParams).

mult(Config, OpParams) ->
    num_op(Config, fun(A,B) -> A*B end, OpParams).

jtrue(#config{position = Position} = Config, OpParams) ->
    Value = get_param(Config, 1, OpParams),
    NewPosition = get_param(Config, 2, OpParams),
    case Value of
        0 -> {ok, Config#config{position = Position + 3}};
        _ -> {ok, Config#config{position = NewPosition}}
    end.

jfalse(#config{position = Position} = Config, OpParams) ->
    Value = get_param(Config, 1, OpParams),
    NewPosition = get_param(Config, 2, OpParams),
    case Value of
        0 -> {ok, Config#config{position = NewPosition}};
        _ -> {ok, Config#config{position = Position + 3}}
    end.

less(#config{tape = Tape, position = Position} = Config, OpParams) ->
    A = get_param(Config, 1, OpParams),
    B = get_param(Config, 2, OpParams),
    WritePosition = get_position(Config, 3, get_mode(OpParams, 3)),
    ToStore = case A<B of
                  true -> 1;
                  _ -> 0
              end,
    NewTape = array:set(WritePosition, ToStore, Tape),
    {ok, Config#config{tape = NewTape, position = Position+4}}.

equals(#config{tape = Tape, position = Position} = Config, OpParams) ->
    A = get_param(Config, 1, OpParams),
    B = get_param(Config, 2, OpParams),
    WritePosition = get_position(Config, 3, get_mode(OpParams, 3)),
    ToStore = case A =:= B of
                  true -> 1;
                  _ -> 0
              end,
    NewTape = array:set(WritePosition, ToStore, Tape),
    {ok, Config#config{tape = NewTape, position=Position+4}}.

stop(_, _) -> stop.

input(Config, _OpParams, []) -> {input, Config}; %% Yield if there is no input ready
input(#config{tape = Tape, position = Position} = Config, OpParams, [InputValue | NewInput]) ->
    WritePosition = get_position(Config, 1, get_mode(OpParams, 1)),
    NewTape = array:set(WritePosition, InputValue, Tape),
    {ok, Config#config{tape = NewTape, position = Position + 2}, NewInput}.

output(#config{tape = Tape, position = Position} = Config, OpParams, Output) ->
    Value = get_param(Config, 1, OpParams),
    io:format("~B~n", [Value]),
    {output, Config#config{tape = Tape, position = Position + 2}, [Value | Output]}.

adjust_rel_base(#config{rel_base = RelBase, position = Position} = Config, OpParams) ->
    Value = get_param(Config, 1, OpParams),
    {ok, Config#config{rel_base = RelBase + Value, position = Position + 2}}.

format_opcode(1) -> "+";
format_opcode(2) -> "*";
format_opcode(3) -> ">>";
format_opcode(4) -> "<<";
format_opcode(5) -> "je";
format_opcode(6) -> "jne";
format_opcode(7) -> "le";
format_opcode(8) -> "eq";
format_opcode(9) -> "adjb";
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

