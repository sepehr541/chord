-module(chord_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/chord_types.hrl").
-import(chord_api, [create/1, join/2]).
-import(chord_api, [rpc_putEntry/3, rpc_getEntry/2]).
-import(chord_utils, [nameToNode/1, toInt/1, extract_state/1]).
-import(chord_ft_utils, [successor/1, ft_node/2, ft_start/2, ft_interval/2]).

-define(BootstrapNode, node1).

printInterval(#interval_Closed_Open{left = Left, right = Right}) ->
    "[" ++ integer_to_list(toInt(Left)) ++ ", " ++ integer_to_list(toInt(Right)) ++ ")".

printFT(State) ->
    #chord_node{id = Id, ref= Ref } = State#state.this,
    ?debugFmt("State: { id = ~p, ref = ~p }~n", [toInt(Id), Ref]),
    ?debugFmt("Index, Start, Interval, Node~n", []),
    array:map(
        fun(Index, _) ->
            K = Index + 1,
            ?debugFmt(
                "~p, ~p, ~p, ~p~n",
                [
                    K,
                    toInt(ft_start(State, K)),
                    printInterval(ft_interval(State, K)),
                    (ft_node(State, K))#chord_node.ref
                ]
            )
        end,
        State#state.ft
    ).

% Function to check the finger table for unit tests
check_finger_table(State = #state{this = ThisNode}, Nodes) ->
    RingSize = 1 bsl ?M,
    lists:foldl(
        fun(Index, Discrepancies) ->
            Start = (toInt(ThisNode#chord_node.id) + (1 bsl (Index - 1))) rem RingSize,
            ExpectedNode = find_closest_successor(Start, Nodes),
            ActualNode = ft_node(State, Index),
            case ActualNode#chord_node.id == ExpectedNode#chord_node.id of
                true -> Discrepancies;
                false -> [{Index, ExpectedNode#chord_node.id, ActualNode#chord_node.id} | Discrepancies]
            end
        end, [], lists:seq(1, ?M)).

% Function to find the closest successor node
find_closest_successor(Start, Nodes) ->
    SortedNodes = lists:sort(fun(#chord_node{id = ID1}, #chord_node{id = ID2}) ->
                                 toInt(ID1) =< toInt(ID2)
                             end, Nodes),
    case lists:dropwhile(fun(#chord_node{id = ID}) -> toInt(ID) < Start end, SortedNodes) of
        [ClosestNode|_] -> ClosestNode;
        [] -> lists:nth(1, SortedNodes)
    end.



setup() ->
    % Assuming create_node/0 initializes and returns a new chord node
    ?assertMatch({ok, _Pid}, create(?BootstrapNode)),
    ?assert(is_process_alive(whereis(?BootstrapNode))),
    ?BootstrapNode.

teardown(NodeName) ->
    % Clean up the node, if necessary
    gen_server:stop(NodeName),
    ok.

create_test() ->
    ?assertMatch({ok, _Pid}, create(test_node)),
    ?assert(is_process_alive(whereis(test_node))),
    gen_server:stop(test_node).

-define(assert_all_same(Array, Value),
    ?assert(array:foldl(fun(_, E, Acc) -> E =:= Value andalso Acc end, true, Array))
).

join_test() ->
    setup(),
    Node1Name = ?BootstrapNode,
    Node2Name = node2,
    Node1 = nameToNode(Node1Name),
    Node2 = nameToNode(Node2Name),

    Node1StateBefore = extract_state(Node1Name),
    ?assert(Node1StateBefore#state.pred =:= Node1),
    ?assert(successor(Node1StateBefore) =:= Node1),
    % ?debugFmt("================ Node1 Before ==================", []),
    % printFT(Node1StateBefore),

    join(Node2Name, Node1Name),

    Node1StateAfter = extract_state(Node1Name),
    Node2StateAfter = extract_state(Node2Name),

    % ?debugFmt("================ Node1 After ==================", []),
    % printFT(Node1StateAfter),

    % ?debugFmt("================ Node2 After ==================", []),
    % printFT(Node2StateAfter),

    

    ?assert(Node1StateAfter#state.pred =:= Node2),
    ?assert(successor(Node1StateAfter) =:= Node2),
    ?assertEqual([], check_finger_table(Node1StateAfter, [Node1, Node2])),

    ?assert(Node2StateAfter#state.pred =:= Node1),
    ?assert(successor(Node2StateAfter) =:= Node1),
    ?assertEqual([], check_finger_table(Node2StateAfter, [Node1, Node2])),

    teardown(Node1Name).


put_entry(NodeName) ->
    % Now use the Node to perform operations
    Key = "test_key",
    Value = "test_value",
    Node = nameToNode(NodeName),

    StateBefore = extract_state(NodeName),
    ?_assert(maps:size(StateBefore#state.kvstore) =:= 0),

    rpc_putEntry(Node, Key, Value),

    StateAfter = extract_state(NodeName),
    Map = StateAfter#state.kvstore,
    ?_assert(maps:size(Map) =:= 1),
    ?_assert(maps:is_key(Key, Map)),
    ?_assert(maps:get(Key, Map) =:= Value).

get_entry_not_in_store(NodeName) ->
    Key = "test_key",
    Node = nameToNode(NodeName),

    StateBefore = extract_state(NodeName),
    ?_assert(maps:size(StateBefore#state.kvstore) =:= 0),

    ReturnValue = rpc_getEntry(Node, Key),

    ?_assert(ReturnValue =:= undefined).

get_entry(NodeName) ->
    Key = "test_key",
    Value = "test_value",
    Node = nameToNode(NodeName),

    StateBefore = extract_state(NodeName),
    ?_assert(maps:size(StateBefore#state.kvstore) =:= 1),

    ReturnValue = rpc_getEntry(Node, Key),

    ?_assertNot(ReturnValue =:= undefined),
    ?_assert(ReturnValue =:= Value).

kvstore_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(SetupData) ->
        [
            get_entry_not_in_store(SetupData),
            put_entry(SetupData),
            get_entry(SetupData)
        ]
    end}.
