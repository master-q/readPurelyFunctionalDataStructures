all: BinomialHeapTest binomial_heap_test_ml

# OCaml
binomial_heap_test_ml: binomial_heap_test_ml.ml binomial_heap_ml.ml
	ocamlfind ocamlc -o binomial_heap_test_ml -package oUnit -linkpkg \
	binomial_heap_ml.ml binomial_heap_test_ml.ml

# Haskell
BinomialHeapTest: BinomialHeap.hs BinomialHeapTest.hs
	ghc --make -Wall BinomialHeapTest.hs

lint:
	hlint -c *.hs

test: all
	@echo "*** Haskell ***"
	./BinomialHeapTest
	@echo "\n*** OCaml ***"
	./binomial_heap_test_ml

clean:
	rm -rf BinomialHeapTest binomial_heap_test_ml
	rm -rf *.hi *.o *.cmi *.cmo a.out
	rm -rf *~
	rm -rf */*~

.PHONY: lint test clean
