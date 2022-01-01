#!/usr/bin/env sh
exec dune exec --profile=release tests/bench1.exe -- $@
