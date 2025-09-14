GHC := ghc
ORMOLU := ormolu

TARGET := timeout
SRC := timeout.hs
GHC_FLAGS := -dynamic -Wall

$(TARGET): $(SRC)
	$(GHC) $(GHC_FLAGS) -o $@ $<

format:
	$(ORMOLU) -i $(SRC)

clean:
	rm -f $(TARGET)
	rm -f *.hi
	rm -f *.o

.PHONY: clean format
