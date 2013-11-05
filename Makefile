all:
	ghc Main.hs -o Main
	clean

clean:
	-rm *.o
	-rm *.hi