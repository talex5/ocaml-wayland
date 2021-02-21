test:
	dune build @check @runtest

run: test
	dune exec -- ./example/test.exe

doc:
	dune build @doc
