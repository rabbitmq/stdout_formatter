-module(stdout_formatter_nifs).

-export([isatty/0,
         isatty/1]).

-on_load(on_load/0).

isatty() ->
    isatty(stdout).

isatty(Fd)
  when Fd =:= stdin orelse
       Fd =:= stdout orelse
       Fd =:= stderr ->
    undefined.

on_load() ->
    Target = erlang_to_llvm_target(get_erlang_target()),
    Lib = filename:join(
            [code:priv_dir(stdout_formatter),
             Target,
             "libstdout_formatter_nifs"]),
    case erlang:load_nif(Lib, 0) of
        ok ->
            ok;
        {error, {_Reason, Text}} ->
            logger:warning("stdout_formatter: Failed to load NIFs: ~s",
                           [Text])
    end,
    ok.

get_erlang_target() ->
    case os:getenv("ERLANG_SYS_ARCH") of
        false -> erlang:system_info(system_architecture);
        Value -> Value
    end.

erlang_to_llvm_target(ErlangTarget) ->
    [Arch, Vendor, OS] = string:split(ErlangTarget, "-", all),
    Arch1 = case Arch of
                "amd64" -> "x86_64";
                _       -> Arch
            end,
    Vendor1 = case Vendor of
                  "portbld" -> "unknown";
                  _         -> Vendor
              end,
    OS1 = re:replace(OS, "[0-9.]*$", "", [{return, list}]),
    string:join([Arch1, Vendor1, OS1], "-").
