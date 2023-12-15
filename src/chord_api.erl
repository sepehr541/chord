-module(chord_api).
-include("chord_types.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(chord_circular_interval, [isInInterval/2]).
-import(chord_ft_utils, [ft_node/2, successor/1, ft_set_finger/3, ft_start/2, ft_interval/2]).
-import(chord_utils, [hash/1, pow/2, mod/2, toInt/1, toBin/1, distance/2, printNode/1]).
-import(script_utils, [printInterval/1]).

-define(GEN_SERVER_MODULE, chord_gen_server).

%
% Local
%
-export([
    create/1,
    join/2,
    findSuccessor/2,
    findPredecessor/2,
    closestPreceedingFinger/2,
    notify/1,
    initFingerTable/2,
    updateFingerTable/3,
    moveKeys/2,
    acceptKVEntires/2,
    updatePredecessor/2
]).

%
% KVStore
%
-export([
    putEntry/3,
    getEntry/2
]).

%
% RPC
%
-export([
    rpc_findSuccessor/2,
    rpc_closestPreceedingFinger/2,
    rpc_putEntry/3,
    rpc_getEntry/2,
    rpc_moveKeys/2,
    rpc_updatePredecessor/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% create a single node (i.e. new ring)
%
-spec create(NodeName :: atom()) -> any().
create(NodeName) ->
    Name = {local, NodeName},
    gen_server:start_link(Name, ?GEN_SERVER_MODULE, [NodeName], []).

%
% create a new node and join an exisiting ring
%
-spec join(NodeName, BootstrapNode) -> any() when
    NodeName :: atom(),
    BootstrapNode :: atom().

join(NodeName, BootstrapNode) ->
    % check if bootstrapNode is registered
    case whereis(BootstrapNode) of
        undefined ->
            error("Bootstrap node does not exist");
        _ ->
            gen_server:start_link(
                {local, NodeName}, ?GEN_SERVER_MODULE, [NodeName, BootstrapNode], []
            )
    end.

%%%
%%%
%%%
%%%
% // ask node n to find the successor of id
% n.find_successor(id)
%     // Yes, that should be a closing square bracket to match the opening parenthesis.
%     // It is a half closed interval.
%     if id ∈ (n, successor] then
%         return successor
%     else
%         // forward the query around the circle
%         n0 := closest_preceding_node(id)
%         return n0.find_successor(id)
%%%
%%%
%%%
%%%
%%%
-spec findSuccessor(State, Id) -> Successor when
    State :: state(),
    Id :: id(),
    Successor :: chord_node().

findSuccessor(#state{this = ThisNode} = State, Id) ->
    Successor = successor(State),
    Predecessor = State#state.pred,
    % io:format("findSuccessor with Id: ~p using This: ~p, Succ: ~p, Pred: ~p~n", 
    %     [toInt(Id), printNode(ThisNode), printNode(Successor), printNode(Predecessor)]),
    case Successor =:= ThisNode of
        true ->
            ThisNode;
        false ->
            IntervalMe = #interval_Open_Closed{
                left = Predecessor#chord_node.id, right = ThisNode#chord_node.id
            },
            case isInInterval(Id, IntervalMe) of
                true ->
                    ThisNode;
                false ->
                    IntervalSucc = #interval_Open_Closed{
                        left = ThisNode#chord_node.id, right = Successor#chord_node.id
                    },
                    case isInInterval(Id, IntervalSucc) of
                        true ->
                            Successor;
                        false ->
                            CPN = closestPreceedingFinger(State, Id),
                            ?assert(CPN =/= ThisNode),
                            rpc_findSuccessor(CPN, Id)
                    end
            end
    end.

%
% find the successor of Id
%
% -spec findSuccessor(State, Id) -> Successor when
%     State :: state(),
%     Id :: id(),
%     Successor :: chord_node().

% findSuccessor(#state{this = ThisNode} = State, Id) ->
%     Nprime = findPredecessor(State, Id),
%     % io:format("N: ~p, Nprime: ~p, Eq?: ~p~n",
%     % [printNode(ThisNode), printNode(Nprime), Nprime =:= ThisNode]),
%     case Nprime =:= ThisNode of
%         true -> successor(State);
%         false -> rpc_successor(Nprime)
%     end.

%
% find the predecessor of Id
%
-spec findPredecessor(State, Id) -> Predecessor when
    State :: state(),
    Id :: id(),
    Predecessor :: chord_node().

findPredecessor(#state{this = Node} = State, Id) ->
    case Node =:= successor(State) of
        % single node in ring
        true -> Node;
        false -> findPredecessorHelper(State, Node, Id)
    end.

-spec findPredecessorHelper(State, Nprime, Id) -> Predecessor when
    State :: state(),
    Nprime :: chord_node(),
    Id :: id(),
    Predecessor :: chord_node().

findPredecessorHelper(#state{this = ThisNode} = State, CurrNprime, Id) ->
    % io:format(
    %     "N: ~p, Nprime: ~p, Eq?: ~p, Id: ~p~n",
    %     [printNode(ThisNode), printNode(CurrNprime), CurrNprime =:= ThisNode, toInt(Id)]
    % ),
    Successor =
        case ThisNode =:= CurrNprime of
            true -> successor(State);
            false -> rpc_successor(CurrNprime)
        end,
    Interval = #interval_Open_Closed{
        left = CurrNprime#chord_node.id, right = Successor#chord_node.id
    },
    case isInInterval(Id, Interval) of
        true ->
            CurrNprime;
        false ->
            % io:format("~p not in ~p~n", [toInt(Id), printInterval(Interval)]),
            UpdatedNprime =
                case CurrNprime =:= ThisNode of
                    true -> closestPreceedingFinger(State, Id);
                    false -> rpc_closestPreceedingFinger(CurrNprime, Id)
                end,
            case UpdatedNprime =:= CurrNprime of
                true -> UpdatedNprime;
                % case
                %     distance(ThisNode#chord_node.id, Id) <
                %         distance(UpdatedNprime#chord_node.id, Id)
                % of
                %     true -> ThisNode;
                %     false ->
                % end;
                false -> findPredecessorHelper(State, UpdatedNprime, Id)
            end
    end.

%
% initialize the finger table of local node
%
% NOTE: uses RPC to BootstrapNode
-spec initFingerTable(State, BootstrapNode) -> UpdatedState when
    State :: state(),
    BootstrapNode :: chord_node(),
    UpdatedState :: state().

initFingerTable(#state{this = #chord_node{id = NodeId} = Node} = State, BootstrapNode) ->
    Successor = rpc_findSuccessor(BootstrapNode, ft_start(State, 1)),
    UpdatedState = ft_set_finger(State, 1, Successor),
    lists:foldl(
        fun(I, StateAcc) ->
            Interval = #interval_Closed_Open{
                left = NodeId, right = ft_start(StateAcc, I)
            },
            FingerStart = ft_start(StateAcc, I + 1),
            case isInInterval(FingerStart, Interval) of
                true ->
                    ft_set_finger(StateAcc, I + 1, ft_node(StateAcc, I));
                false ->
                    FingerSuccessorRPC = rpc_findSuccessor(BootstrapNode, FingerStart),
                    FingerSuccessorRPCId = FingerSuccessorRPC#chord_node.id,
                    D1 = distance(FingerStart, FingerSuccessorRPCId),
                    D2 = distance(FingerStart, NodeId),
                    FingerSuccessor =
                        case D1 < D2 of
                            true -> FingerSuccessorRPC;
                            false -> Node
                        end,
                    ft_set_finger(StateAcc, I + 1, FingerSuccessor)
            end
        end,
        UpdatedState,
        lists:seq(1, ?M - 1)
    ).

-spec notify(State) -> any() when State :: state().

notify(#state{this = Node} = State) ->
    N = toInt(Node#chord_node.id),
    lists:foreach(
        fun(I) ->
            % io:format("Index: ~p~n", [I]),
            Id = toBin(mod(N - pow(2, I - 1), pow(2, ?M))),
            P = findPredecessor(State, Id),
            % io:format("Local: ~p, Index: ~p, Id: ~p, Pred: ~p~n", [
            %     printNode(Node), I, toInt(Id), printNode(P)
            % ]),
            case P =:= Node of
                true -> noop;
                false -> 
                    % io:format("Before == RPC to ~p: updateFingerTable~n", [printNode(P)]),
                    rpc_cast_updateFingerTable(P, Node, I)
                    % io:format("After == RPC to updateFingerTable~n", [])
            end
        end,
        lists:seq(1, ?M)
    ).

%
% Local: return closest finger preceeding Id
%
-spec closestPreceedingFinger(State, Id) -> Finger when
    State :: state(),
    Id :: id(),
    Finger :: chord_node().

closestPreceedingFinger(State, Id) ->
    closestPreceedingFingerHelper(State, Id, ?M).

-spec closestPreceedingFingerHelper(State, Id, Index) -> Finger when
    State :: state(),
    Id :: id(),
    Index :: ftIndex(),
    Finger :: chord_node().

closestPreceedingFingerHelper(#state{this = Node} = State, Id, Index) when Index > 0 ->
    FingerNode = ft_node(State, Index),
    % io:format("CPF for Id ~p using This: ~p, Finger:~p, Index:~p~n",
    %     [toInt(Id), printNode(Node), printNode(FingerNode), Index]),

    FingerNodeId = FingerNode#chord_node.id,
    Interval = #interval_Open_Open{left = Node#chord_node.id, right = Id},
    case isInInterval(FingerNodeId, Interval) of
        true -> FingerNode;
        false when Index =:= 1 -> Node;
        _ -> closestPreceedingFingerHelper(State, Id, Index - 1)
    end.

%
% Local: Update finger table according to the Remote request
% This should be called as a handler for RPC
%
-spec updateFingerTable(State, S, I) -> UpdatedState when
    State :: state(),
    S :: chord_node(),
    I :: ftIndex(),
    UpdatedState :: state().

updateFingerTable(#state{this = ThisNode} = State, S, I) ->
    FingerStart = ft_start(State, I),
    CurrentFinger = ft_node(State, I),
    D1 = distance(FingerStart, CurrentFinger#chord_node.id),
    D2 = distance(FingerStart, S#chord_node.id),
    % io:format("(~p): I: ~p, Start: ~p,  CF: ~p, S: ~p, D1: ~p, D2: ~p~n",
    %     [printNode(ThisNode), I, toInt(FingerStart), printNode(CurrentFinger), printNode(S), D1, D2]),
    case D1 > D2 of
        true ->
            UpdatedState = ft_set_finger(State, I, S),
            P = UpdatedState#state.pred,
            % io:format("updateFinger: ~p RPC to ~p~n", [printNode(ThisNode), printNode(P)]),
            case P =:= ThisNode of
                true -> noop;
                false -> rpc_cast_updateFingerTable(P, S, I)
            end,
            UpdatedState;
        % No-Op
        false ->
            State
    end.

%
% Local: move local keys to Remote
%
-spec moveKeys(State, Remote) -> UpdatedState when
    State :: state(),
    Remote :: chord_node(),
    UpdatedState :: {kvstore(), state()}.

moveKeys(#state{this = #chord_node{id = N}, kvstore = KVStore} = State, Remote) ->
    EntriesToMove = maps:filter(
        fun(Key, _) ->
            Interval = #interval_Closed_Open{left = hash(Key), right = N},
            isInInterval(Remote#chord_node.id, Interval)
        end,
        KVStore
    ),
    EntriesToKeep = maps:without(maps:keys(EntriesToMove), KVStore),
    {EntriesToMove, State#state{kvstore = EntriesToKeep}}.

%
% Local: merge in incoming KVStore entries from Remote
%
-spec acceptKVEntires(State, Entries) -> UpdatedState when
    State :: state(),
    Entries :: kvstore(),
    UpdatedState :: state().

acceptKVEntires(#state{kvstore = KVStore} = State, Entries) ->
    MergedKVStore = maps:merge(KVStore, Entries),
    State#state{kvstore = MergedKVStore}.

-spec updatePredecessor(State, Node) -> {OldPred, UpdatedState} when
    State :: state(),
    Node :: chord_node(),
    OldPred :: chord_node(),
    UpdatedState :: state().

updatePredecessor(State = #state{pred = Pred}, Node) ->
    {Pred, State#state{pred = Node}}.

%
% local: Shutdown Node
%

% TODO
% -spec shutdownNode(Node :: state()) -> ok | error.
% shutdownNode(Node) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Key-Value Store %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Local: put Key-Value in Chord Ring
%
-spec putEntry(State, Key, Value) -> UpdatedState when
    State :: state(),
    Key :: key(),
    Value :: value(),
    UpdatedState :: state().

putEntry(#state{this = ThisNode, kvstore = KVStore} = State, Key, Value) ->
    % io:format("PutEntry(~p, Key = ~p)~n", [printNode(ThisNode), Key]),
    Id = hash(Key),
    Successor = findSuccessor(State, Id),
    % io:format("found Successor ~p ~n", [printNode(Successor)]),

    case Successor =:= ThisNode of
        true ->
            State#state{kvstore = maps:put(Key, Value, KVStore)};
        false ->
            % io:format("redirecting Put to ~p~n", [printNode(Successor)]),
            rpc_putEntry(Successor, Key, Value),
            State
    end.

-spec getEntry(State, Key) -> Entry when
    State :: state(),
    Key :: key(),
    Entry :: value() | undefined.

getEntry(#state{this = ThisNode, kvstore = KVStore} = State, Key) ->
    Successor = findSuccessor(State, hash(Key)),
    case Successor =:= ThisNode of
        true -> maps:get(Key, KVStore, undefined);
        false -> rpc_getEntry(Successor, Key)
    end.

%
% TODO: Concurrent Join Support
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% RPC Calls %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% get the successor of remote node
%
rpc_successor(Remote) ->
    rpc_call(Remote, #successor{}).

%
% Remote: return closest finger preceeding Id
%
rpc_closestPreceedingFinger(Remote, Id) ->
    rpc_call(Remote, #closestPreceedingFinger{targetId = Id}).

%
% ask remote to find the successor of Id
%
-spec rpc_findSuccessor(Remote, Id) -> Successor when
    Remote :: chord_node(),
    Id :: id(),
    Successor :: chord_node().

rpc_findSuccessor(Remote, Id) ->
    rpc_call(Remote, #findSuccessor{targetId = Id}).

%
% Remote: tell remote to update its finger table to Node at index I
%
-spec rpc_cast_updateFingerTable(Remote, Node, I) -> ok when
    Remote :: chord_node(),
    Node :: chord_node(),
    I :: ftIndex().

rpc_cast_updateFingerTable(Remote, Node, I) ->
    rpc_cast(Remote, #updateFingerTable{node = Node, index = I}).

%
% Remote: tell node to store Key-Value
%
-spec rpc_putEntry(Remote, Key, Value) -> ok when
    Remote :: chord_node(),
    Key :: key(),
    Value :: value().

rpc_putEntry(Remote, Key, Value) ->
    rpc_call(Remote, #putEntry{entry = #kvEntry{key = Key, value = Value}}).

%
% Remote: tell node to get Key-Value
%
-spec rpc_getEntry(Remote, Key) -> Entry when
    Remote :: chord_node(),
    Key :: key(),
    Entry :: value() | undefined.

rpc_getEntry(Remote, Key) ->
    rpc_call(Remote, #getEntry{key = Key}).

%
% Ask remote for keys that now belong to this Node
%
-spec rpc_moveKeys(Remote, ThisNode) -> kvstore() when
    Remote :: chord_node(),
    ThisNode :: chord_node().

rpc_moveKeys(Remote, ThisNode) ->
    rpc_call(Remote, #moveKeys{node = ThisNode}).

%
% Tell remote to update its Predecessor to caller Node
%
-spec rpc_updatePredecessor(Remote, ThisNode) -> RemoteOldPred when
    Remote :: chord_node(),
    ThisNode :: chord_node(),
    RemoteOldPred :: chord_node().

rpc_updatePredecessor(Remote, ThisNode) ->
    rpc_call(Remote, #updatePredecessor{node = ThisNode}).

%
% Helpers
%
-spec rpc_call(Remote, Request) -> any() when
    Remote :: chord_node(),
    Request :: any().

rpc_call(#chord_node{ref = Ref}, Request) ->
    gen_server:call(Ref, Request).

-spec rpc_cast(Remote, Request) -> any() when
    Remote :: chord_node(),
    Request :: any().

rpc_cast(#chord_node{ref = Ref}, Request) ->
    gen_server:cast(Ref, Request).
