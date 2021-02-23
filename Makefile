test:
	dune build @all @runtest

run: test
	dune exec -- ./example/test.exe

doc:
	dune build @doc
