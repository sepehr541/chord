#!/usr/bin/env escript
%%! -smp enable -sname script -pa _build/default/lib/chord/ebin

-mode(compile).
-include_lib("stdlib/include/assert.hrl").
-include("../src/chord_types.hrl").

-import(script_utils, [average/1, std/2, printFT/1]).
-import(chord_utils, [extract_state/1, nameToNode/1, toInt/1]).


main([NumNodesStr]) ->
    % Convert the argument to an integer
    NumNodes = list_to_integer(NumNodesStr),

    % Assuming chord_node module is compiled and in the code path
    NodeNames = [list_to_atom("node" ++ integer_to_list(N)) || N <- lists:seq(1, NumNodes)],
    {[Node1], Rest} = lists:split(1, NodeNames),

    % Start node1
    {ok, _} = chord_api:create(Node1),
    % io:format("Node ~p created.\n", [Node1]),

    % Measure join times for subsequent nodes
    JoinTimes = [measure_join_time(Node1, N) || N <- Rest],
    JoinTimeValues = [Time || {_, Time} <- JoinTimes],

    % Calculate average and standard deviation
    Avg = average(JoinTimeValues),
    Std = std(JoinTimeValues, Avg),

    % Output N, average, and standard deviation as a CSV line
    io:format("~p, ~p, ~p~n", [NumNodes, Avg, Std]);
main(_) ->
    io:format("Usage: ./script_name NumNodes\n").

measure_join_time(FirstNode, NodeName) ->
    StartTime = erlang:monotonic_time(),

    {ok, _} = chord_api:join(NodeName, FirstNode),

    EndTime = erlang:monotonic_time(),
    JoinTime = erlang:convert_time_unit(EndTime - StartTime, native, microsecond),

    {NodeName, JoinTime}.
