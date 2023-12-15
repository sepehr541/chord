#!/usr/bin/env escript
%%! -smp enable -sname script -pa _build/default/lib/chord/ebin
-mode(compile).
-include_lib("stdlib/include/assert.hrl").
-include("../src/chord_types.hrl").

-import(script_utils, [create_ring/1, perform_puts/2, perform_gets/2, generate_keys/1]).
-import(script_utils, [average/1, std/2, printFT/1]).
-import(chord_utils, [extract_state/1]).

main([NumNodesStr, NumKeysStr]) ->
    NumNodes = list_to_integer(NumNodesStr),
    NumKeys = list_to_integer(NumKeysStr),

    Nodes = create_ring(NumNodes),
    % lists:foreach(
    %     fun(Node) ->
    %         printFT(Node)
    %     end,
    %     [extract_state(N#chord_node.ref) || N <- Nodes]
    % ),
    % timer:sleep(1000),
    % io:format(lists:duplicate(80, $+) ++ "~n", []),
    % Perform Put operations
    Keys = generate_keys(NumKeys),
    PutTimes = perform_puts(Nodes, Keys),
    ?assert(length(PutTimes) =:= NumKeys),

    % Perform Get operations
    GetTimes = perform_gets(Nodes, Keys),
    ?assert(length(GetTimes) =:= NumKeys),

    % Output results
    printStats("Put", PutTimes),
    printStats("Get", GetTimes);
main(_) ->
    io:format("Usage: ./script NumNodes NumKeys\n").

printStats(Stat, Data) ->
    Avg = average(Data),
    Std = std(Data, Avg),
    io:format("~p Operation:~nAvg(μs), Std(μs)~n~p,~p~n", [Stat, Avg, Std]).
