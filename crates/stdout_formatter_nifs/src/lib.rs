#[macro_use]
extern crate rustler;

extern crate atty;
use atty::Stream;

use rustler::{Env, Term, Error, Encoder};
use rustler::types::atom::Atom;

mod atoms {
    rustler_atoms! {
        atom stdin;
        atom stdout;
        atom stderr;
    }
}

rustler_export_nifs!(
    "stdout_formatter_nifs",
    [("isatty", 1, isatty)],
    None
);

fn isatty<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let stream_name: Atom = args[0].decode()?;

    let stream;

    if stream_name == atoms::stdin() {
        stream = Stream::Stdin;
    } else if stream_name == atoms::stdout() {
        stream = Stream::Stdout;
    } else if stream_name == atoms::stderr() {
        stream = Stream::Stderr;
    } else {
        return Err(Error::BadArg);
    };

    let isatty = atty::is(stream);
    Ok(isatty.encode(env))
}
