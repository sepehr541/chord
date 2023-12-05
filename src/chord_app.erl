%%%-------------------------------------------------------------------
%% @doc chord public API
%% @end
%%%-------------------------------------------------------------------

-module(chord_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([create/1]).

start(_StartType, _StartArgs) ->
    chord_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
%% node.go
-type maybe(T) :: {just, T} | nothing.
-type finger_table() :: [pid(), ...].

-record(cnode, {id, pred, ft}).
-type cnode() :: #cnode{id::binary(), pred::maybe(pid()), ft::finger_table()}.

-type timer() :: ok. % use send_after/2

-spec init_state() -> Node when
    Node::cnode().

init_state() ->
    PidBinary = term_to_binary(self()),
    Id = crypto:hash(sha, PidBinary),
    #cnode{id=Id, pred=null, ft=[Id]}.

-spec create_node() -> none().
create_node() ->
    Node = init_state(),
    handleMessages(Node).


-spec create_and_join(Remote) -> none() | error when
    Remote::pid().

create_and_join(Remote) ->
    Node = init_state(),
    Remote ! {}
    handleMessages(Node).

-spec create(Init) -> pid() when
    Init::fun().

create(Init) ->
    Pid = spawn(Init),
    io:format("Process created: ~p~n", [Pid]),
    Pid.



-spec handleMessages(Node) -> none() when
    Node::cnode().

handleMessages(Node) ->
    receive
        {state} -> io:format("Node: ~p~n~p~n", [self(), Node]);
        {findSuccessor, }
        {stabilize, Pid, Id} -> error;
        {notify_request, Pid, Id} -> error;
        {notify_response, Pid, Id} -> error
    end,
    handleMessages(Node).


% – func (node *Node) join(other *RemoteNode) error
-spec join(Node, RemoteNode) -> ok | error when
    Node::cnode(),
    RemoteNode::cnode().

join(Node, RemoteNode) -> error.

% – func (node *Node) stabilize(ticker *time.Ticker)
-spec stabilize(Node, Timer) -> ok | error when
    Node::cnode(),
    Timer::timer().

stabilize(Node, Timer) -> error.

% – func (node *Node) notify(remoteNode *RemoteNode)
-spec notify(Node, RemoteNode) -> ok | error when
    Node::cnode(),
    RemoteNode::cnode().

notify(Node, RemoteNode) -> error.

% – func (node *Node) findSuccessor(id []byte) (*RemoteNode, error)
-spec findSuccessor(Node, Id) -> ok | error when
    Node::cnode(),
    Id::binary().

findSuccessor(Node, Id) -> error.

% – func (node *Node) findPredecessor(id []byte) (*RemoteNode, error)
-spec findPredecessor(Node, Id) -> ok | error when
    Node::cnode(),
    Id::binary().

findPredecessor(Node, Id) -> error.




% • chord.go
% – func ShutdownNode(node *Node)
-spec shutdownNode(Node::cnode()) -> ok | error.
shutdownNode(Node) -> error.

% • finger_table.go
% – func (node *Node) initFingerTable()
-spec initFingerTable(Node::cnode()) -> ok | error.
initFingerTable(Node) -> error.

% – func (node *Node) fixNextFinger(ticker *time.Ticker)
-spec fixNextFinger(Node::cnode(), Timer::timer()) -> ok | error.
fixNextFinger(Node, Timer) -> error.

% – func fingerMath(n []byte, i int, m int) []byte


% • kv_store.go
% – func Get(node *Node, key string) (string, error)
-spec get(Node::cnode(), Key::string()) -> ok | error.
get(Node, Key) -> error.

% – func Put(node *Node, key string, value string) error
-spec put(Node::cnode(), Key::string(), Value::string()) -> ok | error.
put(Node, Key, Value) -> error.

% – func (node *Node) locate(key string) (*RemoteNode, error)

% – func (node *Node) GetLocal(req *KeyValueReq) (*KeyValueReply, error)

% – func (node *Node) PutLocal(req *KeyValueReq (*KeyValueReply, error)

% – func (node *Node) TransferKeys(req *TransferReq) (*RpcOkay, error)
-type transferReq() :: {cnode(), [string()]}.
-spec transferKeys(Node::cnode(), Request::transferReq()) -> ok | error.
transferKeys(Node, Request) -> error.

% • node_rpc_impl.go
% – func (node *Node) GetSuccessorId(req *RemoteId) (*IdReply, error)
-spec getSuccessorId(Node::cnode(), Remote::pid()) -> binary() | error.
getSuccessorId(Node, Remote) -> error.

% – func (node *Node) SetPredecessorId(req *UpdateReq) (*RpcOkay, error)
-spec setPredecessorId(Node::cnode(), Remote::pid(), Id::binary()) -> ok | error.
setPredecessorId(Node, Remote, Id) -> error.

% – func (node *Node) SetSuccessorId(req *UpdateReq) (*RpcOkay, error)
-spec setSuccessorId(Node::cnode(), Remote::pid(), Id::binary()) -> ok | error.
setSuccessorId(Node, Remote, Id) -> error.

% – func (node *Node) FindSuccessor(query *RemoteQuery) (*IdReply, error)
-spec findSuccessor(Node::cnode(), Remote::pid(), Id::binary()) -> ok | error.
findSuccessor(Node, Remote, Id) -> error.

% – func (node *Node) Notify(remoteNode *RemoteNode) (*RpcOkay, error)
-spec notifyNode(Node::cnode(), Remote::pid()) -> ok | error.
notifyNode(Node, Remote) -> error.


% – func (node *Node) ClosestPrecedingFinger(query *RemoteQuery) (*IdReply, error)
-spec closestPrecedingFinger(Node::cnode(), Remote::pid(), Id::binary()) -> ok | error.
closestPrecedingFinger(Node, Remote, Id) -> error.

% • util.go
% – func Between(nodeX, nodeA, nodeB []byte) bool
% – func BetweenRightIncl(nodeX, nodeA, nodeB []byte) bool