%
% Constants
%
-define(M, 160).

%
% Node Structure
%
-type id() :: binary().

-record(chord_node, {id :: id(), pid :: pid()}).
-type chord_node() :: #chord_node{}.

-type fingertable() :: array:array(chord_node()).

-record(state, {
    this :: chord_node(),
    pred :: (chord_node() | nil),
    ft :: fingertable(),
    next :: pos_integer(),
    kvstore :: kvstore()
}).
-type state() :: #state{}.

%
% Message Structure
%
-type message() :: request() | response().
-record(request, {remote :: chord_node(), payload :: request_payload()}).
-type request() :: #request{}.

-record(response, {remote :: chord_node(), payload :: response_payload()}).
-type response() :: #response{}.

-type message_payload() :: request_payload() | response_payload().
-type request_payload() ::
    findSuccessor()
    | notify()
    | getPredecessor().

-type response_payload() ::
    foundSuccessor()
    | foundPredecessor().

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

-record(foundEntry, {entry :: kvEntry()}).
-type foundEntry() :: #foundEntry{}.

-record(entryNotFound, {}).
-type entryNotFound() :: #entryNotFound{}.

%
% Timer
%

% use send_after/2
-type timer() :: ok.
