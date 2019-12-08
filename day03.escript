#!/usr/bin/env escript

move_dir(_, 0, {X,Y}=Pos, Flavour) ->
    case Flavour of
        lay ->
            put(Pos, 1);
        check ->
            case get(Pos) of
                undefined -> noop;
                X when X >= 1 -> io:format("==+== {~B,~B}, distance: ~B~n", [X, Y, abs(X)+abs(Y)])
            end
    end,
    Pos;
move_dir(Dir, Len, {X,Y}=Pos, Flavour) ->
    case Flavour of
        lay ->
            put(Pos, 1);
        check ->
            case get(Pos) of
                undefined -> noop;
                1 -> io:format("==+== {~B,~B}, distance: ~B~n", [X, Y, abs(X)+abs(Y)])
            end
    end,
    NewPos = case Dir of
        $R -> {X+1,Y};
        $L -> {X-1,Y};
        $U -> {X,Y-1};
        $D -> {X,Y+1}
    end,
    move_dir(Dir, Len-1, NewPos, Flavour).

move([Dir|LenString], {X,Y} = Pos, Flavour)->
    Len = list_to_integer(LenString),
    {NewX, NewY} = NewPos = move_dir(Dir, Len, Pos, Flavour),
    io:format("{~B,~B} ~c -> ~B {~B,~B}~n", [X,Y, Dir, Len, NewX, NewY]),
    NewPos.

lay_wire(Wire, Flavour) ->
    lists:foldl( fun (Op, CurPos) -> move(Op, CurPos, Flavour) end, {0,0}, Wire ).

main([]) ->
    Line = io:get_line(""),
    TrimmedLine = string:trim(Line, both, "\n"),
    FirstWire = string:split(TrimmedLine, ",", all),
    lay_wire(FirstWire, lay),
    SecondLine = io:get_line(""),
    TrimmedSecondLine = string:trim(SecondLine, both, "\n"),
    SecondWire = string:split(TrimmedSecondLine, ",", all),
    lay_wire(SecondWire, check).
