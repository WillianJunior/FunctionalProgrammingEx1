all: clean OpenACCVarLexical.hs OpenACCArgParser.hs OpenACCConstParser.hs
	ghc Main.hs -o Main
	-rm *.o
	-rm *.hi

OpenACCVarLexical.hs: parsers/OpenACCVarParser.x
	alex parsers/OpenACCVarParser.x -o OpenACCVarLexical.hs

OpenACCArgParser.hs: parsers/OpenACCArgParser.y
	happy parsers/OpenACCArgParser.y -o OpenACCArgParser.hs

OpenACCConstParser.hs: parsers/OpenACCConstParser.y
	happy parsers/OpenACCConstParser.y -o OpenACCConstParser.hs

clean:
	-rm *.o
	-rm *.hi