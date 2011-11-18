all: small_stream_test

small_stream_test: small_stream_test.ml small_stream.ml
	ocamlfind ocamlc -o small_stream_test -package oUnit -linkpkg \
	small_stream.ml small_stream_test.ml

test: all
	@echo "*** OCaml ***"
	./small_stream_test

clean:
	rm -rf small_stream_test
	rm -rf *.hi *.o *.cmi *.cmo a.out
	rm -rf *~
	rm -rf */*~

.PHONY: test clean
