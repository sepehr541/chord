-module(chord_utils).

-include("chord_types.hrl").

-export([isIdInRange/3, hash/1]).


-spec isIdInRange(Id, Lower, Upper) -> boolean() when
    Id :: id(),
    Lower :: id(),
    Upper :: id().

isIdInRange(Id, Lower, Upper) when (Lower < Upper) andalso (Lower < Id) andalso (Id < Upper) -> true;
isIdInRange(Id, Lower, Upper) when (Lower > Upper) andalso ((Id < Upper) orelse (Id > Lower)) -> true;
isIdInRange(_, _, _) -> false.


-spec hash(Term) -> Digest when
    Term :: term(),
    Digest :: binary().

hash(Term) ->
    crypto:hash(sha, term_to_binary(Term)).
