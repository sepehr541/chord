-module(chord_ft_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/chord_types.hrl").

%% Test data and setup
-define(TEST_ID_BIN, <<1>>).
-define(TEST_ID, binary_to_integer(?TEST_ID_BIN)).
-define(TEST_NODE, #chord_node{id = ?TEST_ID_BIN}).
-define(TEST_STATE, #state{this = ?TEST_NODE, ft = chord_ft_utils:ft_new(?M, undefined)}).

%% ft_new/2 tests
ft_new_test() ->
    [{setup, fun() -> {ok, array:new([{size, ?M}, {default, undefined}, {fixed, true}])} end,
      fun(_Setup) ->
          ?assertMatch({array, ?M, undefined, _, _, _}, chord_ft_utils:ft_new(?M, undefined))
      end}].

%% ft_start/2 tests
ft_start_test() ->
    StartIndex = 5,
    ExpectedStart = (?TEST_ID + chord_utils:pow(2, StartIndex - 1)) rem chord_utils:pow(2, ?M),
    ?assertEqual(integer_to_binary(ExpectedStart), chord_ft_utils:ft_start(?TEST_STATE, StartIndex)).

%% ft_interval/2 tests
ft_interval_test() ->
    IntervalIndex = 4,
    ExpectedInterval = #interval_Closed_Open{
        left = chord_ft_utils:ft_start(?TEST_STATE, IntervalIndex),
        right = chord_ft_utils:ft_start(?TEST_STATE, IntervalIndex + 1)
    },
    ?assertEqual(ExpectedInterval, chord_ft_utils:ft_interval(?TEST_STATE, IntervalIndex)).

%% ft_node/2 tests
ft_node_test() ->
    Index = 3,
    ExpectedNode = array:get(Index - 1, ?TEST_STATE#state.ft),
    ?assertEqual(ExpectedNode, chord_ft_utils:ft_node(?TEST_STATE, Index)).

%% successor/1 tests
successor_test() ->
    ?assertEqual(chord_ft_utils:ft_node(?TEST_STATE, 1), chord_ft_utils:successor(?TEST_STATE)).

%% ft_set_finger/3 tests
ft_set_finger_test() ->
    Index = 2,
    NewNode = #chord_node{id = <<123>>},
    UpdatedState = chord_ft_utils:ft_set_finger(?TEST_STATE, Index, NewNode),
    ?assertEqual(NewNode, array:get(Index - 1, UpdatedState#state.ft)).

