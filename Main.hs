module Main where
import F95SrcIO ( read_F95_src, write_F95_src )
import F95OpenACCParser -- ( extract_OpenACC_regions_from_F95_src )
import F95VarDeclParser ( run_parser, f95_var_decl_parser )
import F95ParDeclParser ( f95_par_decl_parser )
import F95Types
import OpenCLAPIGenerator ( gen_OpenCL_API_calls )
import EvalExpr    
import qualified Data.Map as H

import OpenACCArgParser
import OpenACCConstParser
import OpenACCParamParser

{-

Sequence of actions:

 1/ detect arguments and argmodes, make a list of arguments

 2/ detect entry for new declarations for buffers and sizes, generate code and insert 

 3/ detect entry for API calls, generate code and insert 

-}

templ_src_name=  "module_LES_ocl_TEMPL.f95"
gen_src_name = "module_LES_ocl.f95"

      
-- ###############################################################################
-- Code for parsing the argument declarations

-- Given the lines containing arguments arg_lines and the lines containing the constant arguments const_arg_lines
-- create a table with as key the variable name and as value the parsed declaration
-- also returns a list of the argument variable names and the constant argument variable names
parse_arg_decls :: [String] -> [String] -> (ArgTable,[String],[String])
parse_arg_decls arg_lines const_arg_lines = (argTable,argsNames,consArgsNames)
	where
		argTable = H.fromList [(name, arg) | name <- names, arg <- parsedLines, elem name $ vd_varlist arg]	
		names = argsNames ++ consArgsNames
		parsedLines = parsedArgLines ++ parsedConstLines
		parsedArgLines = map parseACCArgLine arg_lines
		parsedConstLines = map parseACCConstLine const_arg_lines
		argsNames = foldr (++) [] $ map vd_varlist parsedArgLines
		consArgsNames = foldr (++) [] $ map vd_varlist parsedConstLines

-- Given the parameter declarations, create a table with as key the parameter name and as value the parsed declaration	
parse_par_decls :: [String] -> VarTable
parse_par_decls par_lines = H.fromList $ joinLists (map pd_parname parsedParams) (map pd_parval parsedParams)
	where
		parsedParams = map parseACCParamLine par_lines
		joinLists (x:xs) (y:ys) = (x,y) : joinLists xs ys
		joinLists [] [] = []

-- This takes a range expression and returns a tuple with the variable name and the computed size
-- eval_range_expr :: ArgTable -> VarTable -> String -> (String, [Integer])
-- eval_range_expr ocl_args par_table var_name = (var_name, value)
--	where
--		value = (\(Just justVar) -> [myEval (sizeExpr x) par_table | x <- vd_dimension justVar]) var
--		var = H.lookup var_name ocl_args
--		sizeExpr = \x -> Op $ MkOpExpr "sub" (r_stop x) (r_start x)
 
-- this way we don't need to run through the ArgTable twice (once for every argument and the snd time to find the VarDecl on the ArgTable)
updateArgTableRange :: [(String, VarDecl)] -> VarTable -> [(String, VarDecl)]
updateArgTableRange argTable@((arg_name,arg):args) varTable = (arg_name,newArg) : updateArgTableRange args varTable
	where
		newArg = MkVarDecl (vd_vartype arg) (newDimension) (vd_intent arg) (vd_varlist arg) (vd_argmode arg) (vd_is_arg arg)
		newDimension = [range | range <- map newRange $ vd_dimension arg]
		newRange (MkRange start end) = (MkRange (getNewExpr start $ exprCalc start) (getNewExpr end $ exprCalc end))
		getNewExpr old (Just expr) = Const expr
		getNewExpr old Nothing = old
		exprCalc expr = myEval (Just expr) varTable
updateArgTableRange [] _ = []

-- ###############################

main :: IO ()
main = do 
	srcLines <- read_F95_src templ_src_name
	let (args, consts, params) = extract_OpenACC_regions_from_F95_src srcLines
	let (tempArgTable, argsNames, consArgsNames) = parse_arg_decls args consts
	let varTable = parse_par_decls params
	let argTable = H.fromList $ updateArgTableRange (H.toList argTable) varTable
	let output = gen_OpenCL_API_calls tempArgTable argsNames consArgsNames srcLines []
	write_F95_src gen_src_name output

--        "-- read source template from file"
--        ,"-- extract OpenACC regions"
--        ,"-- parse declarations"
--        ,"-- compute sizes for OpenCL arguments (this is hard, leave for last)"
--        ,"-- generate the target source code" 
--        ,"-- write generated source to file"
