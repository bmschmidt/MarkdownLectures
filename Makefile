DEST=/usr/local/bin

all: $(DEST)/lectureToOutline $(DEST)/lectureToSlidedeck $(DEST)/shuffleAllLists $(DEST)/convertOldFormat clean

$(DEST)/%: src/%.hs
	ghc $<
	mv src/$* $@

clean:
	rm -f src/*.hi src/*.o
