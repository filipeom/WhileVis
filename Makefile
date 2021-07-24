HC 		 = ghc
SRC    = src
TARGET = Main

SRCS := $(wildcard $(SRC)/*.hs)

.PHONY: clean

$(TARGET): $(SRCS)
	@echo "$(HC) $^ -o $@"; $(HC) $^ -o $@

clean: $(SRCS:.hs=.o) $(SRCS:.hs=.hi) $(TARGET)
	$(RM) $^
