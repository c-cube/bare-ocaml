
# Bare-OCaml ![build](https://github.com/c-cube/bare/workflows/build/badge.svg)

A simple code generator and runtime library for [BARE](https://baremessages.org/).

License: MIT.


## Features

- runtime library named `Bare`, with entrypoints for encoding/decoding
  all the base types over `Buffer.t` and `bytes` respectively
- code generator for producing OCaml code with type declarations and encoder/decoder functions.
  (TODO: also generate pretty printers)
  * the code generator can handle mutually recursive types and out-of-order
    declarations, by sorting them first.
- basic testing (needs to be improved when test vectors are published)

- basic vim syntax files in `data/vim/`
