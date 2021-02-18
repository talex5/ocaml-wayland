test:
	dune build @check

run: test
	dune exec -- ./example/test.exe

doc:
	dune build @doc
