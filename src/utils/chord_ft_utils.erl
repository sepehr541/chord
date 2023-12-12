-module(chord_ft_utils).
-include("../chord_types.hrl").
-import(chord_utils, [pow/2]).

-export([ft_new/2, ft_start/2, ft_interval/2, ft_node/2, successor/1, ft_set_finger/3]).

-spec ft_new(Size, DefaultValue) -> fingertable() when
    Size :: pos_integer(),
    DefaultValue :: any().

ft_new(Size, DefaultValue) ->
    array:new([{size, Size}, {default, DefaultValue}, {fixed, true}]).

-spec ft_start(State, K) -> EntryStart when
    State :: state(),
    K :: ftIndex(),
    EntryStart :: id().

ft_start(#state{this = #chord_node{id = N}}, K) when 1 =< K andalso K =< ?M ->
    (N + pow(2, K - 1)) rem pow(2, ?M).

-spec ft_interval(State, K) -> interval_Closed_Open() when
    State :: state(),
    K :: ftIndex().

ft_interval(State, K) when 1 =< K andalso K < ?M ->
    #interval_Closed_Open{left = ft_start(State, K), right = ft_start(State, K + 1)};
ft_interval(#state{this = #chord_node{id = N}} = State, ?M) ->
    #interval_Closed_Open{left = ft_start(State, ?M), right = N}.

-spec ft_node(State, K) -> Node when
    State :: state(),
    K :: ftIndex(),
    Node :: chord_node().

ft_node(#state{ft = Ft}, K) when 1 =< K andalso K =< ?M ->
    array:get(K - 1, Ft).

-spec successor(State) -> Successor when
    State :: state(),
    Successor :: chord_node().

successor(State) ->
    ft_node(State, 1).

-spec ft_set_finger(State, K, Node) -> UpdatedState when
    State :: state(),
    K :: ftIndex(),
    Node :: chord_node(),
    UpdatedState :: state().

ft_set_finger(#state{ft = Ft} = State, K, Node) when 1 =< K andalso K =< ?M ->
    UpdatedFt = array:set(K - 1, Node, Ft),
    State#state{ft = UpdatedFt}.
