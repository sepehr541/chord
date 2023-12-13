#!/usr/bin/env escript
%%! -smp enable -sname script -pa _build/default/lib/chord/ebin

-mode(compile).

-include_lib("stdlib/include/assert.hrl").

main([NumNodesStr | _Rest]) ->
    try
        % Convert the argument to an integer
        NumNodes = list_to_integer(NumNodesStr),

        % Assuming chord_node module is compiled and in the code path
        Node1 = node1,
        
        % Start node1
        {ok, _} = chord_api:create(Node1),
        io:format("Node ~p created.\n", [Node1]),

        % Measure join times for subsequent nodes
        JoinTimes = [measure_join_time(Node1, N) || N <- lists:seq(2, NumNodes)],
        JoinTimeValues = [Time || {_Node, Time} <- JoinTimes],

        % Calculate average and standard deviation
        Avg = average(JoinTimeValues),
        Std = std_dev(JoinTimeValues, Avg),

        % Output N, average, and standard deviation as a CSV line
        io:format("~p,~p,~p\n", [NumNodes, Avg, Std])
    catch
        _:_ ->
            io:format("Usage: ./script_name NumNodes\n")
    end;

main(_) ->
    io:format("Usage: ./script_name NumNodes\n").

measure_join_time(FirstNode, NodeNum) ->
    Node = list_to_atom("node" ++ integer_to_list(NodeNum)),
    StartTime = erlang:monotonic_time(),
    
    {ok, _} = chord_api:join(Node, FirstNode),
    
    EndTime = erlang:monotonic_time(),
    JoinTime = erlang:convert_time_unit(EndTime - StartTime, native, microsecond),

    {Node, JoinTime}.

average(List) ->
    sum(List) / length(List).

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

std_dev(List, Avg) ->
    SqrSum = lists:foldl(fun(X, Acc) -> (X - Avg) * (X - Avg) + Acc end, 0, List),
    math:sqrt(SqrSum / length(List)).

