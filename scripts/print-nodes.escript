#!/usr/bin/env escript
%%! -smp enable -sname script -pa _build/default/lib/chord/ebin

-mode(compile).
-include("../src/chord_types.hrl").

-import(chord_utils, [nameToNode/1, toInt/1]).


main([NumNodesStr]) ->
    % Convert the argument to an integer
    NumNodes = list_to_integer(NumNodesStr),

    % Assuming chord_node module is compiled and in the code path
    NodeNames = [list_to_atom("node" ++ integer_to_list(N)) || N <- lists:seq(1, NumNodes)],
    Nodes = [nameToNode(N) || N <- NodeNames],
    lists:foreach(
        fun(#chord_node{id = Id, ref = Ref}) -> 
            io:format("~p : ~p~n", [Ref, toInt(Id)])
        end,
        Nodes);

main(_) ->
    io:format("Usage: ./script_name NumNodes\n").
