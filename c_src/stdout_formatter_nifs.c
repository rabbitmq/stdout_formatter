#include <string.h>
#include <unistd.h>

#include "erl_nif.h"

static ERL_NIF_TERM
isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret, fd;
    char atom_arg[10];

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    ret = enif_get_atom(env, argv[0], atom_arg, (unsigned int)sizeof(atom_arg),
        ERL_NIF_LATIN1);
    if (ret == 0) {
        return enif_make_badarg(env);
    }

    if (strcmp(atom_arg, "stdin") == 0) {
        fd = 0;
    } else if (strcmp(atom_arg, "stdout") == 0) {
        fd = 1;
    } else if (strcmp(atom_arg, "stderr") == 0) {
        fd = 2;
    } else {
        return enif_make_badarg(env);
    }

    ret = isatty(fd);
    return enif_make_atom(env, ret ? "true" : "false");
}

static ErlNifFunc nif_funcs[] = {
    {"isatty", 1, isatty_nif}
};

ERL_NIF_INIT(stdout_formatter_nifs, nif_funcs, NULL, NULL, NULL, NULL);
