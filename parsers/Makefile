all: var_parser decl_parser param_parser

var_parser:
	alex OpenACCVarParser.x -o OpenACCVarLexical.hs
	happy OpenACCVarParser.y -o OpenACCVarParser.hs

decl_parser:
	alex OpenACCDeclParser.x -o OpenACCDeclLexical.hs
	happy OpenACCDeclParser.y -o OpenACCDeclParser.hs

param_parser:
	alex OpenACCParamParser.x -o OpenACCParamLexical.hs
	happy OpenACCParamParser.y -o OpenACCParamParser.hs