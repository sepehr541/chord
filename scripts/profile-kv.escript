#!/usr/bin/env escript
%%! -smp enable -sname chord_script -pa _build/default/lib/chord/ebin

-mode(compile).

-include_lib("stdlib/include/assert.hrl").

-include("../src/chord_types.hrl").
-import(script_utils, [create_ring/1, generate_keys/2, perform_puts/2]).

main([NumNodesStr, NumKeysStr, KeySizeStr, Output]) ->
    NumNodes = list_to_integer(NumNodesStr),
    NumKeys = list_to_integer(NumKeysStr),
    KeySize = list_to_integer(KeySizeStr),


    % Setup - Create nodes and perform initial Put operations
    Nodes = create_ring(NumNodes),
    Keys = generate_keys(NumKeys, KeySize),

    % Start profiling
    eprof:start(),
    eprof:start_profiling([ Node#chord_node.ref || Node <- Nodes]),

    perform_puts(Nodes, Keys),

    eprof:stop_profiling(),
    eprof:log(Output),
    eprof:analyze(),

    io:format("Profile results written to profile.txt\n");
main(_) ->
    io:format("Usage: ./script NumNodes NumKeys KeySize Output\n").


    
