-module(chord_api_tests).
-include_lib("eunit/include/eunit.hrl").
-import(chord_api, [create/1, join/2]).

create_test() ->
        ?assertMatch({ok, _Pid}, create(test_node)),
        ?assert(is_process_alive(whereis(test_node))),
        gen_server:stop(test_node).

% join_test_() ->
%     [
%         % Setup - Start a bootstrap node
%         fun setup ->
%             {ok, _} = create(bootstrap_node),
%             bootstrap_node
%         end,
%         % Test - Join a new node to the bootstrap node
%         fun(BootstrapNode) ->
%             ?_assertMatch({ok, _Pid}, join(test_node, BootstrapNode)),
%             ?_assert(is_process_alive(whereis(test_node))),
%             gen_server:stop(test_node) % Clean up
%         end,
%         % Teardown - Stop the bootstrap node
%         fun teardown ->
%             gen_server:stop(bootstrap_node)
%         end
%     ],
%     [
%         % Test - Join with a non-existent bootstrap node
%         ?_assertMatch({error, "Bootstrap node does not exist"}, join(test_node, non_existent_node))
%     ].
