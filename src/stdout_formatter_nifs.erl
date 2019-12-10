-module(stdout_formatter_nifs).

-export([isatty/1]).

-on_load(on_load/0).

on_load() ->
    Target = case os:getenv("ERLANG_SYS_ARCH") of
                 false -> erlang:system_info(system_architecture);
                 Value -> Value
             end,
    Lib = filename:join(
            [code:priv_dir(stdout_formatter),
             Target,
             "stdout_formatter_nifs"]),
    case erlang:load_nif(Lib, 0) of
        ok ->
            ok;
        {error, {_Reason, Text}} ->
            logger:warning("stdout_formatter: Failed to load NIFs: ~s",
                           [Text])
    end,
    ok.

isatty(Fd)
  when Fd =:= stdin orelse
       Fd =:= stdout orelse
       Fd =:= stderr ->
    undefined.
