-module(chord_messaging).

-include("chord_types.hrl").

-export([sendRequest/3, sendResponse/3]).

-spec sendMessage(Node, Type, Remote, Payload) -> ok when
    Node::state(),
    Type::request | response,
    Remote::chord_node(),
    Payload::message_payload().

sendMessage(Node, Type, Remote, Payload) ->
    Remote#chord_node.pid ! {Type, Node, Payload},
    ok.

-spec sendRequest(Node, Remote, Payload) -> ok when
    Node::state(),
    Remote::chord_node(),
    Payload::request_payload().

sendRequest(Node, Remote, Payload) ->
    sendMessage(Node, request, Remote, Payload),
    ok.

-spec sendResponse(Node, Remote, Payload) -> ok when
    Node::state(),
    Remote::chord_node(),
    Payload::response_payload().

sendResponse(Node, Remote, Payload) ->
    sendMessage(Node, response, Remote, Payload),
    ok.
