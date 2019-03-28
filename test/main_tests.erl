-module(main_tests).

-include_lib("eunit/include/eunit.hrl").
-include("stdout_formatter.hrl").

formatting_test() ->
    ?assertEqual(
       "string",
       stdout_formatter:to_string(["st", ["rin", $g]])),

    ?assertEqual(
       "5",
       stdout_formatter:to_string(5)),

    ?assertEqual(
       "+-+\n"  %% +-+
       "|a|\n"  %% |a|
       "+-+",   %% +-+
       stdout_formatter:to_string(
         #table{rows = [[a]],
                props = #{border_drawing => ascii}})).
