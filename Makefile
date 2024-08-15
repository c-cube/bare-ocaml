
build:
	@dune build @all

test:
	@dune runtest --force --no-buffer

test-autopromote:
	@dune runtest --force --no-buffer --auto-promote

bench:
	@dune exec --profile=release tests/bench1.exe

clean:
	@dune clean

WATCH?="@install @runtest"
watch:
	@dune build @all -w $(WATCH)
