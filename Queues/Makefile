all: BatchedQueueTest

BatchedQueueTest: BatchedQueue.hs BatchedQueueTest.hs Queue.hs
	ghc --make -Wall BatchedQueueTest.hs

lint:
	hlint -c *.hs

test: BatchedQueueTest
	./BatchedQueueTest

clean:
	rm -rf BatchedQueueTest
	rm -rf *.hi *.o
	rm -rf *~
	rm -rf */*~

.PHONY: lint test clean
