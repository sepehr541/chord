-module(chord_gen_server).
-behaviour(gen_server).

-include("chord_types.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(chord_utils, [hash/1, nameToNode/1]).
-import(chord_ft_utils, [ft_new/2, successor/1, ft_node/2]).
-import(chord_circular_interval, [isInInterval/2]).
-import(script_utils, [printFT/1]).

-import(chord_api, [
    notify/1,
    initFingerTable/2,
    closestPreceedingFinger/2,
    findSuccessor/2,
    findPredecessor/2,
    updateFingerTable/3,
    acceptKVEntires/2,
    putEntry/3,
    getEntry/2,
    moveKeys/2,
    updatePredecessor/2,
    rpc_moveKeys/2,
    rpc_updatePredecessor/2
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
    % io:format("Node ~p Joining ~p~n", [NodeName, BootstrapNodeName]),
    BootstrapNode = nameToNode(BootstrapNodeName),
    
    State = initState(NodeName),
    ThisNode = State#state.this,
    % printFT(State),


    UpdatedState = initFingerTable(State, BootstrapNode),
    % io:format("Node ~p initFingerTable Done~n", [NodeName]),

    % io:format("========================== After initFingerTable ======================"),
    % printFT(UpdatedState),

    Successor = successor(UpdatedState),
    ?assert(Successor =/= ThisNode),

    notify(UpdatedState),
    % io:format("Node ~p notify Done~n", [NodeName]),

    SuccessorOldPred = rpc_updatePredecessor(Successor, ThisNode),
    {_, UpdatedState2} = updatePredecessor(UpdatedState, SuccessorOldPred),
    KVEntries = rpc_moveKeys(Successor, ThisNode),
    UpdatedState3 = acceptKVEntires(UpdatedState2, KVEntries),
    {ok, UpdatedState3}.

%
% handlers
%

handle_call(Request, From, State) ->
    % io:format("From: ~p~n, Request: ~p~nState-Before: ~p~n", [From, Request, State]),
    Reply = case Request of
        #successor{} -> {reply, successor(State), State};
        #closestPreceedingFinger{targetId = Id} ->
            CPF = closestPreceedingFinger(State, Id),
            {reply, CPF, State};
        #findSuccessor{targetId = Id} ->
            Successor = findSuccessor(State, Id),
            {reply, Successor, State};
        % #updateFingerTable{node = Node, index = I} ->
        %     UpdatedState = updateFingerTable(State, Node, I),
        %     {reply, ok, UpdatedState};
        #moveKeys{node = Remote} ->
            {EntriesToMove, UpdatedState} = moveKeys(State, Remote),
            {reply, EntriesToMove, UpdatedState};
        #putEntry{entry = #kvEntry{key = Key, value = Value}} ->
            UpdatedState = putEntry(State, Key, Value),
            {reply, ok, UpdatedState};
        #getEntry{key = Key} ->
            Value = getEntry(State, Key),
            {reply, Value, State};
        #updatePredecessor{node = Node} ->
            {OldPred, UpdatedState} = updatePredecessor(State, Node),
            {reply, OldPred, UpdatedState}
    end,
    % {reply, _, StateAfter} = Reply,
    % io:format("State-After: ~p~n", [StateAfter]),
    %timer:sleep(1000),
    Reply.

handle_cast(Request, State) ->
    case Request of
        #updateFingerTable{node = Node, index = I} ->
            UpdatedState = updateFingerTable(State, Node, I),
            {noreply, UpdatedState}
    end.

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
