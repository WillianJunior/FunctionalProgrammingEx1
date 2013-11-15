all: OpenACCVarLexical.hs OpenACCArgParser.hs OpenACCConstParser.hs OpenACCParamLexical.hs OpenACCParamParser.hs
	ghc Main.hs -o Main
	-rm *.o
	-rm *.hi

OpenACCVarLexical.hs: parsers/OpenACCVarParser.x
	alex parsers/OpenACCVarParser.x -o OpenACCVarLexical.hs

OpenACCArgParser.hs: parsers/OpenACCArgParser.y
	happy parsers/OpenACCArgParser.y -o OpenACCArgParser.hs

OpenACCConstParser.hs: parsers/OpenACCConstParser.y
	happy parsers/OpenACCConstParser.y -o OpenACCConstParser.hs

OpenACCParamLexical.hs: parsers/OpenACCParamParser.x
	alex parsers/OpenACCParamParser.x -o OpenACCParamLexical.hs

OpenACCParamParser.hs: parsers/OpenACCParamParser.y
	happy parsers/OpenACCParamParser.y -o OpenACCParamParser.hs

clean:
	-rm OpenACCVarLexical.hs
	-rm OpenACCArgParser.hs
	-rm OpenACCConstParser.hs
	-rm OpenACCParamLexical.hs
	-rm OpenACCParamParser.hs
	-rm *.o
	-rm *.hi