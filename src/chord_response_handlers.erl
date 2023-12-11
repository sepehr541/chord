-module(chord_response_handlers).

-include("chord_types.hrl").

-export([handleResponse/3]).

-spec handleResponse(Node, Remote, Payload) -> any() when
    Node::state(),
    Remote::chord_node(),
    Payload::response_payload().

handleResponse(Node, Remote, Payload) ->
    case Payload of
        #foundSuccessor{targetId  = Id, successor = Successor} -> todo
    end.
