-module(sif).

-export([checksum/1, load_image/3]).

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
