-module(stdout_formatter_nifs).

-export([isatty/1]).

-include_lib("host_triple/include/code_loading.hrl").

-on_load(on_load/0).

on_load() ->
    ?load_nif_from_host_triple_dir(
       stdout_formatter,
       atom_to_list(?MODULE),
       0).

isatty(Fd)
  when Fd =:= stdin orelse
       Fd =:= stdout orelse
       Fd =:= stderr ->
    undefined.
