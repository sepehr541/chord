#!/usr/bin/env escript
%%! -smp enable -sname script -pa _build/default/lib/chord/ebin
-mode(compile).
-include_lib("stdlib/include/assert.hrl").

-import(chord_api, [join/2, create/1]).
-import(chord_api, [rpc_getEntry/2, rpc_putEntry/3]).
-import(chord_utils, [nameToNode/1]).

-define(KEY_LENGTH, 10).

main([NumNodesStr, NumKeysStr]) ->
    NumNodes = list_to_integer(NumNodesStr),
    NumKeys = list_to_integer(NumKeysStr),

    % Create and join nodes
    Node1 = node1,
    % Start node1
    {ok, _} = create(Node1),
    io:format("Node ~p created.\n", [Node1]),

    JoinedNodes = lists:map(
        fun(N) ->
            Name = list_to_atom("node" ++ integer_to_list(N)),
            join(Name, Node1),
            nameToNode(Name)
        end,
        lists:seq(2, NumNodes)
    ),

    Nodes = [nameToNode(Node1)] ++ JoinedNodes,

    % Perform Put operations
    Keys = generate_random_keys(NumKeys),
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


average(List) ->
    sum(List) / length(List).

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

std(List, Avg) ->
    SqrSum = lists:foldl(fun(X, Acc) -> (X - Avg) * (X - Avg) + Acc end, 0, List),
    math:sqrt(SqrSum / length(List)).

%% @doc Generate M random string keys, each with the same value as the key.
%% @spec generate_random_keys(M :: non_neg_integer()) -> [{string(), string()}].
generate_random_keys(M) ->
    lists:map(fun(_) -> generate_key_value_pair() end, lists:seq(1, M)).

%% @doc Generate a single random key-value pair.
%% @spec generate_key_value_pair() -> {string(), string()}.
generate_key_value_pair() ->
    Key = generate_random_string(?KEY_LENGTH),
    {Key, Key}.

%% @doc Generate a random string of given length.
%% @spec generate_random_string(Length :: non_neg_integer()) -> string().
generate_random_string(Length) ->
    lists:map(fun(_) -> random_char() end, lists:seq(1, Length)).

%% @doc Generate a random character.
%% @spec random_char() -> char().
random_char() ->
    % Generates a random ASCII character from 33 to 126 (printable characters)
    rand:uniform(94) + 32.

perform_for_keys(Fun, Nodes, Keys) ->
    lists:map(
        fun(Key) ->
            Node = select_node(Nodes),
            StartTime = erlang:monotonic_time(),
            % Assuming put_operation/2 is your Put function
            Fun(Node, Key),
            EndTime = erlang:monotonic_time(),
            erlang:convert_time_unit(EndTime - StartTime, native, microsecond)
        end,
        Keys
    ).

perform_puts(Nodes, Keys) ->
    perform_for_keys(fun(Node, Key) -> rpc_putEntry(Node, Key, Key) end, Nodes, Keys).

perform_gets(Nodes, Keys) ->
    perform_for_keys(fun(Node, Key) -> rpc_getEntry(Node, Key) end, Nodes, Keys).

select_node(Nodes) ->
    % Simple random selection; replace with your logic if needed
    lists:nth(rand:uniform(length(Nodes)), Nodes).
