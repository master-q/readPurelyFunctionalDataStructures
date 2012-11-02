GHC := ghc
PROG := MySeqTest

all: ${PROG}
${PROG}: ${PROG}.hs
	${GHC} -Wall ${PROG}.hs

test: ${PROG}
	./${PROG}

clean:
	rm -f *.o *.hi *~ ${PROG}

.PHONY: clean test
