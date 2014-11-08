all: lectureToOutline lectureToSlidedeck clean

%: %.hs
	ghc $@

clean:
	rm *.hi *.o
