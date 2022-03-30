set shell := ["bash", "-c"]

clippy_arguments := ""

@_default:
    just --list --unsorted

# check formatting using `cargo fmt`
check-format:
    cargo fmt --all -- --check

# run clippy
clippy:
    cargo clippy -- {{clippy_arguments}}

# run tests
test:
    cargo test

# run all lints and tests
check-all: check-format clippy test

# format everything using `cargo fmt`
format:
    cargo fmt --all

# count lines with tokei
tokei:
    tokei .
