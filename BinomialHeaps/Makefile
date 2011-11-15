all: BinomialHeapTest

BinomialHeapTest: BinomialHeap.hs BinomialHeapTest.hs
	ghc --make -Wall BinomialHeapTest.hs

lint:
	hlint -c *.hs

test: BinomialHeapTest
	./BinomialHeapTest

clean:
	rm -rf BinomialHeapTest
	rm -rf *.hi *.o
	rm -rf *~
	rm -rf */*~

.PHONY: lint test clean
