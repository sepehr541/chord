-module(chord_gen_server).
-behaviour(gen_server).

-include("chord_types.hrl").
-import(chord_utils, [hash/1, nameToNode/1]).
-import(chord_ft_utils, [ft_new/2, successor/1]).
-import(chord_circular_interval, [isInInterval/2]).

-import(chord_api, [
    notify/1,
    initFingerTable/2,
    closestPreceedingFinger/2,
    findSuccessor/2,
    updateFingerTable/3,
    acceptKVEntires/2,
    putEntry/3,
    getEntry/2
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
    
    % io:format("Node ~p initFingerTable Called~n", [NodeName]),
    UpdatedState = initFingerTable(State, BootstrapNode),
    
    % io:format("Node ~p notify Called~n", [NodeName]),
    notify(UpdatedState),
    
    % io:format("Node ~p init DONE~n", [NodeName]),
    {ok, UpdatedState}.

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
        #updateFingerTable{node = Node, index = I} ->
            UpdatedState = updateFingerTable(State, Node, I),
            {reply, ok, UpdatedState};
        #acceptKVEntires{entries = Entries} ->
            UpdatedState = acceptKVEntires(State, Entries),
            {reply, ok, UpdatedState};
        #putEntry{entry = #kvEntry{key = Key, value = Value}} ->
            UpdatedState = putEntry(State, Key, Value),
            {reply, ok, UpdatedState};
        #getEntry{key = Key} ->
            Value = getEntry(State, Key),
            {reply, Value, State}
    end,
    % {reply, _, StateAfter} = Reply,
    % io:format("State-After: ~p~n", [StateAfter]),
    %timer:sleep(1000),
    Reply.

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
