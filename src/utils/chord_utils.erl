-module(chord_utils).

-include("../chord_types.hrl").

-export([hash/1]).

-spec hash(Term) -> Digest when
    Term :: term(),
    Digest :: binary().

hash(Term) ->
    crypto:hash(sha, term_to_binary(Term)).


-spec pow(Base, Exp) -> Result when 
    Base::integer(),
    Exp::non_neg_integer(),
    Result::non_neg_integer().

pow(Base, Exp) ->
    pow(Base, Exp, 1).

pow(_, 0, Acc) -> Acc;
pow(Base, Exp, Acc) when Exp > 0 -> pow(Base, Exp - 1, Acc * Base).
