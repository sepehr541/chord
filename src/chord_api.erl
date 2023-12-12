-module(chord_api).
-include("chord_types.hrl").
-import(chord_circular_interval, [isInInterval/2]).
-import(chord_ft_utils, [ft_node/2, successor/1, ft_set_finger/3, ft_start/2]).
-import(chord_utils, [pow/2]).

-export([
    create/1,
    join/2,
    findSuccessor/2,
    rpc_findSuccessor/2,
    closestPreceedingFinger/2,
    rcp_closestPreceedingFinger/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% create a single node (i.e. new ring)
%
-spec create(NodeName :: atom()) -> any().
create(NodeName) ->
    Name = {local, NodeName},
    gen_server:start_link(Name, ?MODULE, [NodeName], []).

-spec join(NodeName, BootstrapNode) -> any() when
    NodeName :: atom(),
    BootstrapNode :: atom().

%
% create a new node and join an exisiting ring
%
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
    BootstrapNode :: atom(),
    UpdatedState :: state().

initFingerTable(#state{this = Node} = State, BootstrapNode) ->
    Successor = rpc_findSuccessor(BootstrapNode, ft_start(State, 1)),
    UpdatedState = ft_set_finger(State, 1, Successor),
    lists:foldl(
        fun(I, StateAcc) -> 
            Interval = #interval_Closed_Open{left = Node#chord_node.id, right = ft_start(StateAcc, I)},
            case isInInterval(ft_start(State, I + 1), Interval) of
                true -> ft_set_finger(StateAcc, I + 1, ft_node(State, I));
                false -> ft_set_finger(StateAcc, I + 1, rpc_findSuccessor(BootstrapNode, ft_start(StateAcc, I + 1)))
            end
        end,
        UpdatedState,
        lists:seq(1, ?M - 1)).

-spec notify(State) -> any() when State :: state().

notify(#state{this = #chord_node{id = N}}) ->
    [N - pow(2, I - 1) || I <- lists:seq(1, ?M)].

%
% Local: return closest finger preceeding Id
%
-spec closestPreceedingFinger(State, Id) -> Finger when
    State :: state(),
    Id :: id(),
    Finger :: chord_node().

closestPreceedingFinger(State, Id) ->
    closestPreceedingFinger(State, Id, ?M).

-spec closestPreceedingFinger(State, Id, Index) -> Finger when
    State :: state(),
    Id :: id(),
    Index :: ftIndex(),
    Finger :: chord_node().

closestPreceedingFinger(#state{this = Node}, _, 0) ->
    Node;
closestPreceedingFinger(#state{this = Node, ft = Ft} = State, Id, Index) when Index > 0 ->
    FingerNode = ft_node(Index, Ft),
    FingerNodeId = FingerNode#chord_node.id,
    Interval = #interval_Open_Open{left = Node#chord_node.id, right = Id},
    case isInInterval(FingerNodeId, Interval) of
        true -> FingerNode;
        false -> closestPreceedingFinger(State, Id, Index - 1)
    end.

%%
%% RPC Calls
%%

%
% get the successor of remote node
%
rpc_successor(Remote) ->
    rpc_call(Remote, #successor{}).

%
% Remote: return closest finger preceeding Id
%
rcp_closestPreceedingFinger(Remote, Id) ->
    rpc_call(Remote, #closestPreceedingFinger{targetId = Id}).

%
% ask remote to find the successor of Id
%
-spec rpc_findSuccessor(Remote, Id) -> Successor when
    Remote :: atom(),
    Id :: id(),
    Successor :: chord_node().

rpc_findSuccessor(Remote, Id) ->
    rpc_call(Remote, #findSuccessor{targetId = Id}).

%
% Helpers
%
-spec rpc_call(Remote, Request) -> any() when
    Remote :: chord_node(),
    Request :: any().

rpc_call(#chord_node{pid = Pid}, Request) ->
    gen_server:call(Pid, Request).
