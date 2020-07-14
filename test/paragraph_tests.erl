%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(paragraph_tests).

-include_lib("eunit/include/eunit.hrl").
-include("stdout_formatter.hrl").

formatting_test() ->
    ?assertEqual(
       "",
       stdout_formatter_paragraph:to_string("")),

    ?assertEqual(
       "string",
       stdout_formatter_paragraph:to_string("string")),

    ?assertEqual(
       "string",
       stdout_formatter_paragraph:to_string(["st", ["rin", $g]])),

    ?assertEqual(
       "A       BBBB    C",
       stdout_formatter_paragraph:to_string("A\tBBBB\tC")),

    ?assertEqual(
       "atom",
       stdout_formatter_paragraph:to_string(atom)),

    ?assertEqual(
       "binary",
       stdout_formatter_paragraph:to_string(<<"binary">>)),

    ?assertEqual(
       "1",
       stdout_formatter_paragraph:to_string(1)),

    ?assertEqual(
       "1.200000",
       stdout_formatter_paragraph:to_string(1.2)),

    ?assertEqual(
       "{my_tuple,a,b}",
       stdout_formatter_paragraph:to_string({my_tuple, a, b})),

    ?assertEqual(
       "1.20",
       stdout_formatter_paragraph:to_string(#paragraph{content = 1.2,
                                             props = #{format => "~.2.0f"}})),

    ?assertEqual(
       "",
       stdout_formatter_paragraph:to_string(#table{})),

    ?assertEqual(
       "\e(0lqqqqqk\e(B\n"
       "\e(0x\e(Btable\e(0x\e(B\n"
       "\e(0mqqqqqj\e(B",
       stdout_formatter_paragraph:to_string(#table{rows = [[table]]})),

    ?assertEqual(
       "Hello World",
       stdout_formatter_paragraph:to_string(
         #formatted_block{
            lines = [#formatted_line{
                        content = ["Hello World"],
                        props = #{reformat_ok => [{["Hello World"], 11}],
                                  width => 11}}],
            props = #{height => 1, width => 11}})).

concatenation_test() ->
    ?assertEqual(
       "helloworld",
       stdout_formatter_paragraph:to_string(["hello", "world"])),

    ?assertEqual(
       "helloworld",
       stdout_formatter_paragraph:to_string([#paragraph{content = "hello"},
                                        "world"])).

colors_test() ->
    ?assertEqual(
       "\033[34mBlue fg\033[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Blue fg",
                                             props = #{fg => blue}})),

    ?assertEqual(
       "\e[38;5;202mOrange fg\e[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Orange fg",
                                             props = #{fg => 202}})),

    ?assertEqual(
       "\033[43mYellow bg\033[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Yellow bg",
                                             props = #{bg => yellow}})),

    ?assertEqual(
       "\e[48;5;202mOrange bg\e[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Orange bg",
                                             props = #{bg => 202}})),

    ?assertEqual(
       "\033[31;42mRed on green\033[0m",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Red on green",
                    props = #{fg => red,
                              bg => green}})),

    ?assertEqual(
       "\033[38;5;253m\033[48;5;54mGrey on purple\033[0m",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Grey on purple",
                    props = #{fg => 253,
                              bg => 54}})),

    ?assertEqual(
       "\033[38;2;3;57;108m\033[48;2;100;151;177mSome blues\033[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Some blues",
                                             props = #{fg => {3, 57, 108},
                                                       bg => {100, 151, 177}
                                                      }})),

    ?assertEqual(
       "\033[1mBold\033[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Bold",
                                             props = #{bold => true}})),

    ?assertEqual(
       "\033[1m\033[31;42mBold + colors\033[0m",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Bold + colors",
                    props = #{bold => true,
                              fg => red,
                              bg => green}})),
    ?assertEqual(
       #formatted_block{
          lines = [#formatted_line{
                      content = ["\e[1;33mYellow!\e[0m"],
                      props = #{reformat_ok => [{["\e[1;33mYellow!\e[0m"],7}],
                                width => 7}}],
          props = #{height => 1,width => 7}},
       stdout_formatter_paragraph:format("\e[1;33mYellow!\e[0m")).

wrap_test() ->
    ?assertEqual(
       "H\ne\nl\nl\no\nw\no\nr\nl\nd",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 1}})),

    ?assertEqual(
       "He\nll\no\nwo\nrl\nd",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 2}})),

    ?assertEqual(
       "Hel\nlo\nwor\nld",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 3}})),

    ?assertEqual(
       "Hell\no\nworl\nd",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 4}})),

    ?assertEqual(
       "Hello\nworld",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 5}})),

    ?assertEqual(
       "Hello\nworld",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 6}})),

    ?assertEqual(
       "Hello\nworld",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 10}})),

    ?assertEqual(
       "Hello world",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 11}})),

    ?assertEqual(
       "Hello world",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = "Hello world",
                    props = #{wrap_at => 12}})).

colors_and_wrap_test() ->
    ?assertEqual(
       "\033[34mBlue\033[0m\n"
       "\033[37mWhite\033[0m\n"
       "\033[31mRed\033[0m\n"
       "\033[34m-----\033[0m",
       stdout_formatter_paragraph:to_string(
         #paragraph{content = ["Blue ",
                               #paragraph{content = "White ",
                                          props = #{fg => white}},
                               #paragraph{content = "Red ",
                                          props = #{fg => red}},
                               "-----"],
                    props = #{fg => blue,
                              wrap_at => 5}})).

format_test() ->
    ?assertEqual(
       #formatted_block{
          lines = [#formatted_line{
                      content = ["Hello World"],
                      props = #{reformat_ok => [{["Hello World"], 11}],
                                width => 11}}],
          props = #{height => 1, width => 11}},
       stdout_formatter_paragraph:format("Hello World")).

display_test() ->
    ?assertEqual(ok, stdout_formatter_paragraph:display("Hello World!")).
