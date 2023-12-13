-module(chord_circular_interval_tests).
-include_lib("eunit/include/eunit.hrl").

-include("src/chord_types.hrl").

-import(chord_circular_interval, [isInInterval/2]).

% Test cases
isInInterval_test_() ->
    % Open-Open Interval
    {inparallel, [
        ?_assert(isInInterval(<<5>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
        ?_assertNot(isInInterval(<<3>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
        ?_assertNot(isInInterval(<<7>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<12>>, #interval_Open_Open{left = <<10>>, right = <<5>>})),
        ?_assertNot(isInInterval(<<8>>, #interval_Open_Open{left = <<10>>, right = <<5>>})),

        % Open-Closed Interval
        ?_assert(isInInterval(<<5>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
        ?_assertNot(isInInterval(<<3>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<7>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<4>>, #interval_Open_Closed{left = <<10>>, right = <<5>>})),

        % Closed-Closed Interval
        ?_assert(isInInterval(<<5>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<7>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<7>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
        ?_assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
        ?_assert(isInInterval(<<8>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
        ?_assert(isInInterval(<<2>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
        ?_assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<3>>, right = <<3>>})),

        % Closed-Open Interval
        ?_assert(isInInterval(<<5>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<3>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
        ?_assertNot(isInInterval(<<7>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
        ?_assert(isInInterval(<<10>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
        ?_assertNot(isInInterval(<<5>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
        ?_assert(isInInterval(<<11>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
        ?_assert(isInInterval(<<4>>, #interval_Closed_Open{left = <<10>>, right = <<5>>}))
    ]}.
