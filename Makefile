GHC := ghc
ORMOLU := ormolu

TARGET := timeout
SRC := timeout.hs
CBITS := cbits.c
# --install-signal-handlers=no keeps the RTS from replacing most inherited
# signal dispositions at startup; SIGINT is still overridden by the GHC
# top handler before main, which is why cbits.c snapshots dispositions
# in a constructor
GHC_FLAGS := -dynamic -threaded -Wall "-with-rtsopts=--install-signal-handlers=no"

$(TARGET): $(SRC) $(CBITS)
	$(GHC) $(GHC_FLAGS) -o $@ $(SRC) $(CBITS)

format:
	$(ORMOLU) -i $(SRC)

clean:
	rm -f $(TARGET)
	rm -f *.hi
	rm -f *.o

.PHONY: clean format
