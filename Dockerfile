FROM rust:buster

RUN apt-get update && apt-get install time clang libclang-dev llvm -y
RUN rustup toolchain install nightly
RUN rustup target add wasm32-unknown-unknown --toolchain nightly
RUN command -v wasm-gc || cargo +nightly install --git https://github.com/alexcrichton/wasm-gc --force

WORKDIR /generic_parachain

# Ideally, we could just do something like `COPY . .`, but that doesn't work:
# it busts the cache every time non-source files like inject_bootnodes.sh change,
# as well as when non-`.dockerignore`'d transient files (*.log and friends)
# show up. There is no way to exclude particular files, or write a negative
# rule, using Docker's COPY syntax, which derives from go's filepath.Match rules.
#
# We can't combine these into a single big COPY operation like
# `COPY collator consensus network runtime test Cargo.* .`, because in that case
# docker will copy the _contents_ of each directory into the image workdir,
# not the actual directory. We're stuck just enumerating them.
COPY . .

RUN cargo build --release
