
# Bare-OCaml ![build](https://github.com/c-cube/bare/workflows/build/badge.svg)

A simple code generator and runtime library for [BARE](https://baremessages.org/).

License: MIT.

## Limitations

- the code generator assumes that declarations in the IDL follow topological
  ordering; i.e., types are defined before they are used, as in OCaml.
