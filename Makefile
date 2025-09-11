GHC := ghc
ORMOLU := ormolu

TARGET := timeout
SRC := timeout.hs

$(TARGET): $(SRC)
	$(GHC) -o $@ $<

format:
	$(ORMOLU) -i $(SRC)

clean:
	rm -f $(TARGET)
	rm -f *.hi
	rm -f *.o

.PHONY: clean format