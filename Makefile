all: clean
	ghc Main.hs -o Main

clean:
	-rm *.o
	-rm *.hi