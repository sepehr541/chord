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

%
% gen_server callbacks
%
-spec init(Args) -> Result when
    Args :: list(atom()),
    Result :: any().

init([NodeName]) ->
    State = initState(NodeName),
    {ok, State};
init([NodeName, BootstrapNode]) ->
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

handle_cast(_, _) ->
    perror(?FUNCTION_NAME).

handle_info(_, _) ->
    perror(?FUNCTION_NAME).

%
% Terminate
%
terminate(_, _) ->
    todo.

%
% Code Change
%
code_change(_, _, _) ->
    perror(?FUNCTION_NAME).

%
% helpers
%
-spec initState(NodeName) -> state() when
    NodeName :: atom().

initState(NodeName) ->
    This = #chord_node{id = hash(NodeName), pid = NodeName},
    Ft = ft_new(?M, This),
    #state{this = This, pred = This, ft = Ft}.

perror(Op) ->
    io:format("Operation not allowed: ~p~n", [Op]).
