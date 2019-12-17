-module(sif).

-export([render/3, checksum/1, load_image/3]).

checksum(Layers) ->
    LayersWithCountedZeros = [{L, length(lists:filter(fun(E) -> E =:= 0 end, L))} || L <- Layers],
    [{LeastZeros,_} | _] = lists:keysort(2, LayersWithCountedZeros),
    {ok, length(lists:filter(fun(E) -> E =:= 1 end, LeastZeros))
         * length(lists:filter(fun(E) -> E =:= 2 end, LeastZeros))}.

%% Would be faster to check the correct length once at the beginning
make_layers(_, _, Acc, []) -> {ok, lists:reverse(Acc)};
make_layers(W, H, _, PixelsLeft) when length(PixelsLeft) < W*H -> {error, wrong_num_of_pixels};
make_layers(W, H, Acc, PixelsLeft) ->
    {Layer, NewPixelsLeft} = lists:split(W*H, PixelsLeft),
    make_layers(W, H, [Layer | Acc], NewPixelsLeft).

load_image(FileName, W, H) ->
    {ok, IO} = file:open(FileName, [read]),
    Line = io:get_line(IO,""),
    TrimmedLine = string:trim(Line, both, "\n"),
    Pixels = [X-$0 || X <- TrimmedLine],
    {ok, _Layers} = R = make_layers(W, H, [], Pixels),
    R.

render(Layers, W, H) ->
    {ok, Image} = compute_image(Layers),
    print_image(Image, W, H).

compute_layer([], [], Acc) -> lists:reverse(Acc);
compute_layer([L|Layer], [I|Img], Acc) ->
    A = case {L,I} of
        {_, Filled} when Filled =:= 0 orelse Filled =:= 1 -> Filled;
        {X, _} -> X
    end,
    compute_layer(Layer, Img, [A|Acc]).

compute_image([First|_] = Layers) ->
    InitialImg = [2 || _ <- lists:seq(0,length(First)-1)],
    {ok, lists:foldl(fun (L, Acc) ->
                        compute_layer(L, Acc, [])
                     end, InitialImg, Layers)}.

inner_print_image(_, 0, 0, _W, _H) -> io:nl(), ok;
inner_print_image(Data, 0, Y, W, H) ->
    io:put_chars([$\n]),
    inner_print_image(Data, W, Y-1, W, H);
inner_print_image([Pix|Rest], X, Y, W, H) ->
    Char = case Pix of
               0 -> $#;
               1 -> $.;
               2 -> $ 
           end,
    io:put_chars([Char]),
    inner_print_image(Rest, X-1, Y, W, H).

print_image(Img, W, H) ->
    inner_print_image(Img, W, H-1, W, H).
