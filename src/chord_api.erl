-module(chord_api).
-include("chord_types.hrl").
-import(chord_circular_interval, [isInInterval/2]).
-import(chord_ft_utils, [ft_node/2, successor/1, ft_set_finger/3, ft_start/2]).
-import(chord_utils, [hash/1, pow/2, mod/2]).

%
% Local
%
-export([
    create/1,
    join/2,
    findSuccessor/2,
    closestPreceedingFinger/2,
    notify/1,
    initFingerTable/2,
    updateFingerTable/3,
    moveKeys/2,
    acceptKVEntires/2
]).

%
% RPC
%
-export([
    rpc_findSuccessor/2,
    rpc_closestPreceedingFinger/2,
    rpc_acceptKVEntires/2
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
    gen_server:start_link(Name, ?MODULE, [NodeName], []).


%
% create a new node and join an exisiting ring
%
-spec join(NodeName, BootstrapNode) -> any() when
    NodeName :: atom(),
    BootstrapNode :: atom().

join(NodeName, BootstrapNode) ->
    % check if bootstrapNode is registered
    case whereis(BootstrapNode) of
        undefined -> error("Bootstrap node does not exist");
        _ -> gen_server:start_link({local, NodeName}, ?MODULE, [NodeName, BootstrapNode], [])
    end.

%
% find the successor of Id
%
-spec findSuccessor(State, Id) -> Successor when
    State :: state(),
    Id :: id(),
    Successor :: chord_node().

findSuccessor(State, Id) ->
    Nprime = findPredecessor(State, Id),
    rpc_findSuccessor(Nprime, Id).

%
% find the predecessor of Id
%
-spec findPredecessor(State, Id) -> Predecessor when
    State :: state(),
    Id :: id(),
    Predecessor :: chord_node().

findPredecessor(#state{this = Node} = State, Id) ->
    case Node =:= successor(State) of
        true -> Node;
        false -> findPredecessor(State, Node, Id)
    end.

-spec findPredecessor(State, Nprime, Id) -> Predecessor when
    State :: state(),
    Nprime :: chord_node(),
    Id :: id(),
    Predecessor :: chord_node().

% local
findPredecessor(#state{this = Local} = State, Local, Id) ->
    Successor = successor(State),
    findPredecessorCommon(State, Local, Successor, Id);
% remote
findPredecessor(State, Remote, Id) ->
    Successor = rpc_successor(Remote),
    findPredecessorCommon(State, Remote, Successor, Id).

findPredecessorCommon(State, Node, Successor, Id) ->
    Interval = #interval_Open_Closed{left = Node#chord_node.id, right = Successor#chord_node.id},
    case isInInterval(Id, Interval) of
        true -> Node;
        false -> findPredecessor(State, Successor, Id)
    end.

%
% initialize the finger table of local node
%
% NOTE: uses RPC to BootstrapNode
-spec initFingerTable(State, BootstrapNode) -> UpdatedState when
    State :: state(),
    BootstrapNode :: chord_node(),
    UpdatedState :: state().

initFingerTable(#state{this = Node} = State, BootstrapNode) ->
    Successor = rpc_findSuccessor(BootstrapNode, ft_start(State, 1)),
    UpdatedState = ft_set_finger(State, 1, Successor),
    lists:foldl(
        fun(I, StateAcc) ->
            Interval = #interval_Closed_Open{
                left = Node#chord_node.id, right = ft_start(StateAcc, I)
            },
            case isInInterval(ft_start(State, I + 1), Interval) of
                true ->
                    ft_set_finger(StateAcc, I + 1, ft_node(State, I));
                false ->
                    ft_set_finger(
                        StateAcc, I + 1, rpc_findSuccessor(BootstrapNode, ft_start(StateAcc, I + 1))
                    )
            end
        end,
        UpdatedState,
        lists:seq(1, ?M - 1)
    ).

-spec notify(State) -> any() when State :: state().

notify(#state{this = Node} = State) ->
    N = binary_to_integer(Node#chord_node.id),
    lists:foreach(
        fun(I) ->
            Id = integer_to_binary(mod(N - pow(2, I - 1), pow(2, ?M))),
            P = findPredecessor(State, Id),
            rpc_updateFingerTable(P, Node, I)
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

closestPreceedingFingerHelper(#state{this = Node, ft = Ft} = State, Id, Index) when Index > 0 ->
    FingerNode = ft_node(Index, Ft),
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
    Finger = ft_node(State, I),
    Interval = #interval_Closed_Open{left = ThisNode#chord_node.id, right = Finger#chord_node.id},
    case isInInterval(S#chord_node.id, Interval) of
        true -> 
            UpdatedState = ft_set_finger(State, I, S),
            P = UpdatedState#state.pred,
            rpc_updateFingerTable(P, S, I),
            UpdatedState;
        false -> State % No-Op
    end.


%
% Local: move local keys to Remote
%
-spec moveKeys(State, Remote) -> UpdatedState when
    State::state(),
    Remote::chord_node(),
    UpdatedState::state().


moveKeys(#state{this = #chord_node{id = N}, kvstore = KVStore} = State, Remote) ->
    EntriesToMove = maps:filter(
        fun(Key, _) ->
            Interval = #interval_Closed_Open{left = hash(Key), right = N},
            isInInterval(Remote#chord_node.id, Interval)
        end,
        KVStore),
    rpc_acceptKVEntires(Remote, EntriesToMove),
    EntriesToKeep = maps:without(maps:keys(EntriesToMove), KVStore),
    State#state{kvstore = EntriesToKeep}.

%
% Local: merge in incoming KVStore entries from Remote
%
-spec acceptKVEntires(State, Entries) -> UpdatedState when
    State::state(),
    Entries::kvstore(),
    UpdatedState::state().

acceptKVEntires(#state{kvstore = KVStore} = State, Entries) ->
    MergedKVStore = maps:merge(KVStore, Entries),
    State#state{kvstore = MergedKVStore}.


%
% local: Shutdown Node
%
-spec shutdownNode(Node :: state()) -> ok | error.
shutdownNode(Node) -> error.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Key-Value Store %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Local: put Key-Value in Chord Ring
%
-spec putKV(State, Key, Value) -> UpdatedState when
    State::state(),
    Key::key(),
    Value::value(),
    UpdatedState::state().

putKV(#state{this = ThisNode} = State, Key, Value) ->
    Successor = findSuccessor(State, hash(Key)),
    case Successor =:= ThisNode of
        true -> State#state{};
        false -> 
            
            State
    end.


%
% TODO: Concurrent Join Support
%

% -spec stabilize(State) -> UpdatedState when
%     State :: state(),
%     UpdatedState :: state().

% stabilize(State) ->
%     Node = State#state.this,
%     Ft = State#state.ft,
%     Successor = array:get(0, Ft),
%     PredOfSucc = getPredecessor(Node, Successor),
%     UpdatedState =
%         case
%             isIdInRange(
%                 PredOfSucc#chord_node.id,
%                 Node#chord_node.id,
%                 Successor#chord_node.id
%             )
%         of
%             true -> State#state{ft = array:set(0, PredOfSucc, Ft)};
%             false -> State
%         end,
%     notify(Node, array:get(0, UpdatedState#state.ft)),
%     UpdatedState.

%
% Helpers
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
-spec rpc_updateFingerTable(Remote, Node, I) -> ok when
    Remote :: chord_node(),
    Node :: chord_node(),
    I :: ftIndex().

rpc_updateFingerTable(Remote, Node, I) ->
    rpc_call(Remote, #updateFingerTable{node = Node, index = I}).


%
% Remote: tell remote to accept Key-Value Entries
%
-spec rpc_acceptKVEntires(Remote, Entries) -> any() when
    Remote::chord_node(),
    Entries::kvstore().


rpc_acceptKVEntires(Remote, Entries) ->
    rpc_call(Remote, #acceptKVEntires{entries = Entries}).

%
% Helpers
%
-spec rpc_call(Remote, Request) -> any() when
    Remote :: chord_node(),
    Request :: any().

rpc_call(#chord_node{ref = Ref}, Request) ->
    gen_server:call(Ref, Request).
