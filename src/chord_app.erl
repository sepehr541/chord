%%%-------------------------------------------------------------------
%% @doc chord public API
%% @end
%%%-------------------------------------------------------------------

-module(chord_app).

-behaviour(application).

-include("chord_types.hrl").

-export([start/2, stop/1]).

-import(chord_request_handlers, [handleRequest/3, findSuccessor/2]).
-import(chord_utils, [isIdInRange/3, hash/1]).

start(_StartType, _StartArgs) ->
    chord_sup:start_link().

stop(_State) ->
    ok.

-spec pidToNode(Pid) -> Node when
    Pid :: pid(),
    Node :: chord_node().

pidToNode(Pid) ->
    #chord_node{id = hash(Pid), pid = Pid}.

% – func (node *Node) stabilize(ticker *time.Ticker)
-spec stabilize(State) -> UpdatedState when
    State :: state(),
    UpdatedState :: state().

stabilize(State) ->
    Node = State#state.this,
    Ft = State#state.ft,
    Successor = array:get(0, Ft),
    PredOfSucc = getPredecessor(Node, Successor),
    UpdatedState =
        case
            isIdInRange(
                PredOfSucc#chord_node.id,
                Node#chord_node.id,
                Successor#chord_node.id
            )
        of
            true -> State#state{ft = array:set(0, PredOfSucc, Ft)};
            false -> State
        end,
    notify(Node, array:get(0, UpdatedState#state.ft)),
    UpdatedState.

-spec fixFingers(State) -> UpdatedState when
    State :: state(),
    UpdatedState :: state().

fixFingers(#state{this = This, ft = Ft, next = Next} = State) ->
    N = This#chord_node.id,
    Step = integer_to_binary(floor(math:pow(2, Next - 1))),
    UpdatedFt = array:set(
        Next,
        findSuccessor(State, N + Step),
        Ft
    ),
    State#state{ft = UpdatedFt, next = (Next rem ?M) + 1}.

% – func (node *Node) notify(remoteNode *RemoteNode)
-spec notify(State, Remote) -> any() when
    State :: state(),
    Remote :: chord_node().

notify(State, Remote) ->
    Node = State#state.this,
    sendRequest(Node, Remote, #notify{}).

% – func (node *Node) findPredecessor(id []byte) (*RemoteNode, error)
-spec getPredecessor(Node, Remote) -> chord_node() | nil when
    Node :: state(),
    Remote :: chord_node().

getPredecessor(State, Remote) ->
    sendRequest(State#state.this, Remote, #getPredecessor{}),
    receive
        #foundPredecessor{predecessor = Pred} -> Pred
    end.

% • chord.go
% – func ShutdownNode(node *Node)
-spec shutdownNode(Node :: state()) -> ok | error.
shutdownNode(Node) -> error.

% • finger_table.go
% – func (node *Node) initFingerTable()
-spec initFingerTable(Node :: state()) -> ok | error.
initFingerTable(Node) -> error.

% – func fingerMath(n []byte, i int, m int) []byte

% • kv_store.go
% – func Get(node *Node, key string) (string, error)

% – func Put(node *Node, key string, value string) error
-spec put(Node :: state(), Key :: string(), Value :: string()) -> ok | error.
put(Node, Key, Value) -> error.

% – func (node *Node) locate(key string) (*RemoteNode, error)

% – func (node *Node) GetLocal(req *KeyValueReq) (*KeyValueReply, error)

% – func (node *Node) PutLocal(req *KeyValueReq (*KeyValueReply, error)

% – func (node *Node) TransferKeys(req *TransferReq) (*RpcOkay, error)
-spec transferKeys(Node :: state(), Request :: chord_node()) -> ok | error.
transferKeys(Node, Request) -> error.

% • node_rpc_impl.go
% – func (node *Node) GetSuccessorId(req *RemoteId) (*IdReply, error)
-spec getSuccessorId(Node :: state(), Remote :: pid()) -> binary() | error.
getSuccessorId(Node, Remote) -> error.

% – func (node *Node) SetPredecessorId(req *UpdateReq) (*RpcOkay, error)
-spec setPredecessorId(Node :: state(), Remote :: pid(), Id :: binary()) -> ok | error.
setPredecessorId(Node, Remote, Id) -> error.

% – func (node *Node) SetSuccessorId(req *UpdateReq) (*RpcOkay, error)
-spec setSuccessorId(Node :: state(), Remote :: pid(), Id :: binary()) -> ok | error.
setSuccessorId(Node, Remote, Id) -> error.

% – func (node *Node) Notify(remoteNode *RemoteNode) (*RpcOkay, error)
-spec notifyNode(Node :: state(), Remote :: pid()) -> ok | error.
notifyNode(Node, Remote) -> error.

% • util.go
% – func Between(nodeX, nodeA, nodeB []byte) bool
% – func BetweenRightIncl(nodeX, nodeA, nodeB []byte) bool

% The effect of network topolgy on Chord's perfomance
% mesh vs star vs ring with start in between
%
%
