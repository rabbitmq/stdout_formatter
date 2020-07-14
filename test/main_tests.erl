%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

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

display_test() ->
    ?assertEqual(ok, stdout_formatter:display("Hello World!")).
