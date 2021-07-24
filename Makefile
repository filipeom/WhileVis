CC = ghc

SRC := $(wildcard *.hs)

.PHONY: clean

Main: $(SRC)
	$(CC) $^

clean: $(SRC:.hs=.o) $(SRC:.hs=.hi) Main
	$(RM) $^
