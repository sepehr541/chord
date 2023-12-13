-module(chord_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/chord_types.hrl").
-import(chord_api, [create/1, join/2]).
-import(chord_api, [rpc_putEntry/3, rpc_getEntry/2]).
-import(chord_utils, [nameToNode/1]).
-import(chord_ft_utils, [successor/1]).

-define(BootstrapNode, node1).

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

join_test(Node1Name) ->
    Node2Name = node2,
    Node1 = nameToNode(Node1Name),
    Node2 = nameToNode(Node2Name),

    Node1StateBefore= extract_state(Node1Name),
    ?_assert(Node1StateBefore#state.pred =:= Node1),
    ?_assert(successor(Node1StateBefore) =:= Node1),

    join(Node2Name, Node1Name),

    Node1StateAfter = extract_state(Node1Name),
    Node2StateAfter = extract_state(Node2Name),

    ?_assert(Node1StateAfter#state.pred =:= Node2),
    ?_assert(successor(Node1StateAfter) =:= Node2),
    
    ?_assert(Node2StateAfter#state.pred =:= Node1),
    ?_assert(successor(Node2StateAfter) =:= Node1).


join_test_() ->
    {setup, 
        fun setup/0, 
        fun teardown/1,
        fun join_test/1}.


extract_state(NodeName) ->
    sys:get_state(whereis(NodeName)).



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
    io:format("ReturnValue: ~p~n", [ReturnValue]),

    ?_assert(ReturnValue == Value).

kvstore_test_() ->
    {setup, fun setup/0, fun teardown/1,
    fun(SetupData) ->
        [
            get_entry_not_in_store(SetupData),
            put_entry(SetupData),
            get_entry(SetupData)
        ]
    end}.
