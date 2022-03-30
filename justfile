set shell := ["bash", "-c"]

@_default:
    just --list --unsorted

# check formatting using `cargo fmt`
check-format:
    cargo fmt --all -- --check

# run clippy
clippy:
    # cargo clippy -- -D warnings
    cargo clippy

# run tests
test:
    cargo test

# run all lints and tests
check-all: check-format clippy test

# format everything using `cargo fmt`
format:
    cargo fmt --all
