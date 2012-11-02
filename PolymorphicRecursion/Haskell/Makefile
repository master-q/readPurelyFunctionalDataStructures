GHC := ghc
PROG := MySeq

all: ${PROG}
${PROG}: ${PROG}.hs
	${GHC} -Wall ${PROG}.hs

clean:
	rm -f *.o *.hi *~ ${PROG}

.PHONY: clean
