-module(chord_request_handlers_tests).
-include_lib("eunit/include/eunit.hrl").

-include("src/chord_types.hrl").

-import(chord_request_handlers, [findSuccessor/2]).
-import(chord_utils, [isIdInRanger/3]).

%
% Helpers
%

mock_pid() ->
    spawn(fun() ->
        receive
        after infinity -> ok
        end
    end).

kill_pid(Pid) ->
    exit(Pid, kill).

% Helper function to create a mock node structure
-define(CREATE_MOCK_NODE_STRUCTURE(CurrentNodeId, SuccessorNodeId, TargetId), begin
    MockPid1 = mock_pid(),
    MockPid2 = mock_pid(),
    CurrentNode = #chord_node{id = CurrentNodeId, pid = MockPid1},
    SuccessorNode = #chord_node{id = SuccessorNodeId, pid = MockPid2},
    Node = #state{this = CurrentNode, pred = nil, ft = array:from_list([SuccessorNode]), next = 1},
    {Node, CurrentNode, TargetId, MockPid1, MockPid2}
end).

-define(CLEANUP_MOCK_PIDS(MockPid1, MockPid2), begin
    kill_pid(MockPid1),
    kill_pid(MockPid2)
end).

%
% findSuccessor
%
successor_is_immediate_next_node_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<20>>, <<30>>, <<25>>),

    ExpectedResult = #foundSuccessor{targetId = TargetId, successor = #chord_node{id = <<30>>, pid = MockPid2}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).

successor_is_itself_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<20>>, <<20>>, <<10>>),

    ExpectedResult = #foundSuccessor{targetId = TargetId, successor = #chord_node{id = <<20>>, pid = MockPid1}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).

target_id_smaller_than_current_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<20>>, <<30>>, <<10>>),

    ExpectedResult = {askNode, TargetId, #chord_node{id = <<30>>, pid = MockPid2}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).

target_and_current_id_greater_than_successor_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<15>>, <<5>>, <<20>>),

    ExpectedResult = #foundSuccessor{targetId = TargetId, successor = #chord_node{id = <<5>>, pid = MockPid2}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).

target_id_equals_successors_id_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<1>>, <<5>>, <<5>>),

    ExpectedResult = #foundSuccessor{targetId = TargetId, successor = #chord_node{id = <<5>>, pid = MockPid2}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).

wrap_around_successor_test() ->
    {Node, CurrentNode, TargetId, MockPid1, MockPid2} =
        ?CREATE_MOCK_NODE_STRUCTURE(<<80>>, <<20>>, <<10>>),

    ExpectedResult = #foundSuccessor{targetId = TargetId, successor = #chord_node{id = <<20>>, pid = MockPid2}},
    ?assertEqual(ExpectedResult, findSuccessor(Node, TargetId)),

    ?CLEANUP_MOCK_PIDS(MockPid1, MockPid2).
