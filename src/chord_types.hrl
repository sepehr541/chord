%
% Constants
%
-define(M, 160).

%
% Node Structure
%
-type id() :: binary().
-type ref() :: pid() | atom().

-record(chord_node, {id :: id(), ref :: ref()}).
-type chord_node() :: #chord_node{}.

-type fingertable() :: array:array(chord_node()).
-type ftIndex() :: 1..?M.

-record(state, {
    this :: chord_node(),
    pred  = nil :: (chord_node() | nil),
    ft :: fingertable(),
    next = 1 :: pos_integer(),
    kvstore = #{} :: kvstore()
}).
-type state() :: #state{}.

%
% RPC Requests
%
-record(findSuccessor, {targetId :: id()}).
-type findSuccessor() :: #findSuccessor{}.

-record(foundSuccessor, {targetId :: id(), successor :: chord_node()}).
-type foundSuccessor() :: #foundSuccessor{}.

-record(askNode, {targetId :: id(), nodeToAsk :: chord_node()}).
-type askNode() :: #askNode{}.

-record(getPredecessor, {}).
-type getPredecessor() :: #getPredecessor{}.
-record(foundPredecessor, {predecessor :: chord_node()}).
-type foundPredecessor() :: #foundPredecessor{}.

-record(notify, {}).
-type notify() :: #notify{}.

-record(heartbeat, {}).
-type heartbeat() :: #heartbeat{}.

-record(heartbeatAck, {}).
-type heartbeatAck() :: #heartbeatAck{}.

-record(closestPreceedingFinger, {targetId :: id()}).
-type closestPreceedingFinger() :: #closestPreceedingFinger{}.

-record(successor, {}).
% -type successor() :: #successor{}.

-record(updateFingerTable, {node :: chord_node(), index :: ftIndex()}).

-record(acceptKVEntires, {entries:: kvstore()}).

%
% Key-Value Store
%
-type key() :: string().
-type value() :: string().
-type kvstore() :: #{key() => value()}.

-record(kvEntry, {key :: key(), value :: value()}).
-type kvEntry() :: #kvEntry{}.

-record(getEntry, {key :: key()}).
-type getEntry() :: #getEntry{}.

-record(putEntry, {entry :: kvEntry()}).
-type putEntry() :: #putEntry{}.

%
% Timer
%

% use send_after/2
-type timer() :: ok.


%
% Interval
%

-record(interval_Open_Open, {left :: id(), right :: id()}).
-type interval_Open_Open() :: #interval_Open_Open{}.

-record(interval_Open_Closed, {left :: id(), right :: id()}).
-type interval_Open_Closed() :: #interval_Open_Closed{}.

-record(interval_Closed_Open, {left :: id(), right :: id()}).
-type interval_Closed_Open() :: #interval_Closed_Open{}.

-record(interval_Closed_Closed, {left :: id(), right :: id()}).
-type interval_Closed_Closed() :: #interval_Closed_Closed{}.

-type interval() ::
    interval_Open_Open()
    | interval_Open_Closed()
    | interval_Closed_Open()
    | interval_Closed_Closed().

