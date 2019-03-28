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
                                             props = #{format => "~.2.0f"}})).

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
       "\033[43mYellow bg\033[0m",
       stdout_formatter_paragraph:to_string(#paragraph{content = "Yellow bg",
                                             props = #{bg => yellow}})),

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
                              bg => green}})).

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
