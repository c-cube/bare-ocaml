
build:
	@dune build @all

test:
	@dune runtest --force --no-buffer

clean:
	@dune clean

WATCH?="@install @runtest"
watch:
	@dune build @all -w $(WATCH)
