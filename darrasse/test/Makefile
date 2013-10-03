all: prog

%: %.hs
	ghc --make -i../src $<

clean:
	rm -f test prog *.o *.hi
