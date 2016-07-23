# rustsym [![Build Status](https://travis-ci.org/trixnz/rustsym.svg)](https://travis-ci.org/trixnz/rustsym)

A tool to query symbols from rust code for use in IDEs

## Installation
```
cargo install rustsym
```

## Running
`rustsym` supports three modes of querying symbols:
* `Global` Searches the crate root for any `*.rs` source files and appends the collected symbols to the final list.
* `Local` Searches a specific `*.rs` file for symbols and skips child modules.
* `LocalChildren` The **default** search. This searches a specific `*.rs` file and any child modules required by the file.

For example, to find symbols matching `foo` in `bar.rs` (excluding child modules), you should invoke `rustsym` as follows: `rustsym search -l bar.rs foo`. Omitting the `-l` will search child modules.

If you run `rustsym search -g . foo` then you will search all source files under the current directory for `foo`.

`rustsym` also supports pretty printing the AST for debugging why certain symbols may not appear, but to also improve the turn around time of additional symbol types. This functionality is entirely provided by [syntex](https://github.com/serde-rs/syntex). It can be invoked with `rustsym --dump file.rs`.
