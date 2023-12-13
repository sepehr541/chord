-module(chord_gen_server).
-behaviour(gen_server).

-include("chord_types.hrl").
-import(chord_utils, [hash/1]).
-import(chord_ft_utils, [ft_new/2, successor/1]).
-import(chord_circular_interval, [isInInterval/2]).

-import(chord_api, [
    notify/1,
    initFingerTable/2,
    closestPreceedingFinger/2,
    findSuccessor/2,
    updateFingerTable/3,
    acceptKVEntires/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-spec nameToNode(Name) -> Node when
    Name :: atom(),
    Node :: chord_node().

nameToNode(Name) ->
    #chord_node{id = hash(Name), ref = Name}.



%
% gen_server callbacks
%
-spec init(Args) -> Result when
    Args :: [atom()],
    Result :: any().

init([NodeName]) ->
    State = initState(NodeName),
    {ok, State};
init([NodeName, BootstrapNodeName]) ->
    BootstrapNode = nameToNode(BootstrapNodeName),
    State = initState(NodeName),
    UpdatedState = initFingerTable(State, BootstrapNode),
    notify(UpdatedState),
    {ok, UpdatedState}.

%
% handlers
%
handle_call(#successor{}, _From, State) ->
    Successor = successor(State),
    {reply, Successor, State};
handle_call(#closestPreceedingFinger{targetId = Id}, _, State) ->
    CPF = closestPreceedingFinger(State, Id),
    {reply, CPF, State};
handle_call(#findSuccessor{targetId = Id}, _, State) ->
    Successor = findSuccessor(State, Id),
    {reply, Successor, State};
handle_call(#updateFingerTable{node = Node, index = I}, _, State) ->
    UpdatedState = updateFingerTable(State, Node, I),
    {reply, ok, UpdatedState};
handle_call(#acceptKVEntires{entries = Entries}, _, State) ->
    UpdatedState = acceptKVEntires(State, Entries),
    {reply, ok, UpdatedState}.

handle_cast(_, State) ->
    perror(?FUNCTION_NAME),
    {noreply, State}.

handle_info(_, State) ->
    perror(?FUNCTION_NAME),
    {noreply, State}.

%
% Terminate
%
terminate(_, _) ->
    todo.

%
% Code Change
%
code_change(_, State, _) ->
    perror(?FUNCTION_NAME),
    {ok, State}.

%
% helpers
%
-spec initState(NodeName) -> state() when
    NodeName :: atom().

initState(NodeName) ->
    This = #chord_node{id = hash(NodeName), ref = NodeName},
    Ft = ft_new(?M, This),
    #state{this = This, pred = This, ft = Ft, next = 1}.

perror(Op) ->
    io:format("Operation not allowed: ~p~n", [Op]).
