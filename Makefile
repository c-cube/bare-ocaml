
build:
	@dune build @all

test:
	@dune runtest --force --no-buffer

clean:
	@dune clean

watch:
	@dune build @all -w
