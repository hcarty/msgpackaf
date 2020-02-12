.PHONY: all test benchmark doc repl clean

all:
	dune build

test:
	dune runtest

benchmark:
	dune build benchmark/bench.exe --profile=release
	dune exec benchmark/bench.exe

doc:
	dune build @doc

repl:
	dune utop src

clean:
	dune clean

fmt:
	dune build @fmt
