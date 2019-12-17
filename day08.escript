#!/usr/bin/env escript
%% The image you received is 25 pixels wide and 6 pixels tall.
%%
%% To make sure the image wasn't corrupted during transmission, the Elves would like you to find the layer that contains the fewest 0 digits. On that layer, what is the number of 1 digits multiplied by the number of 2 digits?


main([FileName]) ->
    true = code:add_pathz("./ebin"),
    {ok, Layers} = sif:load_image(FileName, 25, 6),
    {ok, CheckSum} = sif:checksum(Layers),
    io:format("~w~n", [CheckSum]),
    ok.
