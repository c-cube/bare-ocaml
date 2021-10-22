
# Bare-OCaml ![build](https://github.com/c-cube/bare/workflows/build/badge.svg)

A simple code generator and runtime library for [BARE](https://baremessages.org/)
following [the spec](https://datatracker.ietf.org/doc/draft-devault-bare).

License: MIT.


## Features

- runtime library named `Bare`, with entrypoints for encoding/decoding
  all the base types over `Buffer.t` and `bytes` respectively.

- code generator for producing OCaml code with type declarations and encoder/decoder functions.
  * the code generator can handle mutually recursive types and out-of-order
    declarations, by sorting them first.
    Each type gets its own toplevel module; for example `type foo int`
    will become:

    ```ocaml
    module Int = struct
      type t = int64

      let decode = …
      let encode = …
    end
    ```
  * pretty-printers can be generated using the `--pp` option
  * the generated code can be made dependency-free by using `--standalone`.
    In that case, the code for the `Bare` module will be inserted at the beginning
    of the generated code.
- basic testing (needs to be improved when test vectors are published)

- basic vim syntax files in `data/vim/`
