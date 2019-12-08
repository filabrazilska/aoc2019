#!/usr/bin/env escript

move_dir(_, 0, {X,Y}=Pos, CurrLen, Flavour) ->
    case Flavour of
        lay ->
            put(Pos, CurrLen);
        check ->
            case get(Pos) of
                undefined -> noop;
                L when L >= 0 -> io:format("==+== {~B,~B}, distance: ~B~n", [X, Y, L+CurrLen])
            end
    end,
    {Pos, CurrLen};
move_dir(Dir, Len, {X,Y}=Pos, CurrLen, Flavour) ->
    case Flavour of
        lay ->
            put(Pos, CurrLen);
        check ->
            case get(Pos) of
                undefined -> noop;
                L when L >= 0 -> io:format("==+== {~B,~B}, distance: ~B~n", [X, Y, L+CurrLen])
            end
    end,
    NewPos = case Dir of
        $R -> {X+1,Y};
        $L -> {X-1,Y};
        $U -> {X,Y-1};
        $D -> {X,Y+1}
    end,
    move_dir(Dir, Len-1, NewPos, CurrLen+1, Flavour).

move([Dir|LenString], {{X,Y} = Pos, CurrLen}, Flavour)->
    Len = list_to_integer(LenString),
    {{NewX, NewY} = NewPos, NewLen} = move_dir(Dir, Len, Pos, CurrLen, Flavour),
    io:format("{~B,~B} ~c -> ~B {~B,~B}, total: ~B~n", [X,Y, Dir, Len, NewX, NewY, NewLen]),
    {NewPos, NewLen}.

lay_wire(Wire, Flavour) ->
    lists:foldl( fun (Op, Cur) -> move(Op, Cur, Flavour) end, {{0,0}, 0}, Wire ).

main([]) ->
    Line = io:get_line(""),
    TrimmedLine = string:trim(Line, both, "\n"),
    FirstWire = string:split(TrimmedLine, ",", all),
    lay_wire(FirstWire, lay),
    SecondLine = io:get_line(""),
    TrimmedSecondLine = string:trim(SecondLine, both, "\n"),
    SecondWire = string:split(TrimmedSecondLine, ",", all),
    lay_wire(SecondWire, check).
