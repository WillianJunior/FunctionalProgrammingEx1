{-

 Student Identification
 Name: Willian de Oliveira Barreiros Junior
 Matriculation Number: 2105514
 Course: Functional Programming 4
 Exercise Title: Assessed Exercise 1 (Mandatory): 
    Parsing, Code Generation and State Manipulation 
    in Haskell: a Real-world Application
 Date: 21/11/2013

 Status Report
 The code is compiling without any error, and as far as it
 was tested is working. To help the understanding of the
 code, most of the functions have comments.

 In order to make the main function easy to test and find
 bugs (on development stage) let clauses were used, in an
 imperarive way.

 The only aspect that needs to be emphasized regards the
 eval_range_expr function. When implementing the code
 I strugled to find an equally efficient and eficatious
 to implement the size computation part of the assignment.
 Considering that the code generator funcion don't 
 receive the list of calculated dimension values (that
 probably have the signature [(String,[Integer])] in 
 accordance to the eval_range_expr function signature),
 I'm suposed to either update the argument table or just
 not use the generation-time computed dimension sizes.

 Being that said, if I was to implement eval_range_expr
 I would need to implement another function to iterate
 through the argument table, calculating (whenever possible)
 the sizes, and updating it. This would not only be 
 ineficient but also complicated. In order to avoid this
 I implemented a single function that given the ArgTable 
 and the VarTable returns an updated ArgTable with the 
 calculated values of dimensions (whenever possible).

 The last thing to have in mind is that the evaluator
 didn't support this implementation, so the evaluator
 was also changed, returning a Maybe Integer instead 
 of just an Integer. The details behind the implemetation
 is covered on EvalExpr.hs. With Maybe the implementation
 is more explicit on the way it treats the evaluation.

 -}

module Main where
import F95SrcIO ( read_F95_src, write_F95_src )
import F95OpenACCParser ( extract_OpenACC_regions_from_F95_src )
import F95VarDeclParser ( run_parser, f95_var_decl_parser )
import F95ParDeclParser ( f95_par_decl_parser )
import F95Types
import OpenCLAPIGenerator ( gen_OpenCL_API_calls )
import EvalExpr    
import qualified Data.Map as H
import Data.Char (toLower)

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
		parsedArgLines = map parseACCArgLine arg_lines   -- parse arguments returning [VarDecl]
		parsedConstLines = map parseACCConstLine const_arg_lines   -- parse constant arguments returning [VarDecl]
		argsNames = foldr (++) [] $ map vd_varlist parsedArgLines
		consArgsNames = foldr (++) [] $ map vd_varlist parsedConstLines

-- Given the parameter declarations, create a table with as key the parameter name and as value the parsed declaration	
parse_par_decls :: [String] -> VarTable
parse_par_decls par_lines = H.fromList $ joinLists (map pd_parname parsedParams) (map pd_parval parsedParams)
	where
		parsedParams = map parseACCParamLine par_lines
		joinLists (x:xs) (y:ys) = (x,y) : joinLists xs ys
		joinLists [] [] = []

-- !!! this function (eval_range_expr) makes necessary to run through ArgTable 3 times O(n*(n+1)/2), 
-- n=num of arguments (not even counting the const args!) !!!
-- for each argName in an argsNames (this is done by the caller of eval_range_expr) -> run n times | O(n)
--    lookup the argName into ArgTable -> search can take O((n+1)/2)
--        finally, on the caller function, use the tuple to update the arg

-- This takes a range expression and returns a tuple with the variable name and the computed size
eval_range_expr :: ArgTable -> VarTable -> String -> (String, [Integer])
eval_range_expr ocl_args par_table var_name = ("", [])
--	where
--		value = (\(Just justVar) -> [myEval (sizeExpr x) par_table | x <- vd_dimension justVar]) var
--		var = H.lookup var_name ocl_args
--		sizeExpr = \x -> Op $ MkOpExpr "sub" (r_stop x) (r_start x)
 
-- this way we don't need to run through the ArgTable twice (once for every argument and the snd time to find the VarDecl on the ArgTable)
updateArgTableRange :: [(String, VarDecl)] -> VarTable -> [(String, VarDecl)]
updateArgTableRange argTable@((arg_name,arg):args) varTable = (arg_name,newArg) : updateArgTableRange args varTable
	where
		newArg = MkVarDecl (vd_vartype arg) (newDimension) -- update the argument with the new dimension (if possible)
			(vd_intent arg) (vd_varlist arg) (vd_argmode arg) (vd_is_arg arg) 
		newDimension = [range | range <- map newRange $ vd_dimension arg] -- update all ranges in a given dimension
		newRange (MkRange start end) = 				-- given a range returns a new range with either the old expressions (not posible to calculate)
			(MkRange (getNewExpr start $ exprCalc start) (getNewExpr end $ exprCalc end))  -- or the Integer new values
		getNewExpr old (Just expr) = Const expr -- given an old expression, and the return result of exprCalc
		getNewExpr old Nothing = old            -- return either the calculated value (if it was possible) or the old
		exprCalc expr = myEval (Just expr) varTable -- evaluate an expression, returning a (Just Integer) if the
												    -- evaluation is possible or Nothing if it isn't
updateArgTableRange [] _ = []

-- ###############################

main :: IO ()
main = do 
	srcLines <- read_F95_src templ_src_name -- read source template from file
	let lcSrcLines = map (map toLower) srcLines -- create a lowercase srcLine
	let (args, consts, params) = extract_OpenACC_regions_from_F95_src lcSrcLines -- extract OpenACC regions
	let (tempArgTable, argsNames, consArgsNames) = parse_arg_decls args consts -- parse declarations
	let varTable = parse_par_decls params -- compute sizes for OpenCL arguments (this is hard, leave for last)
	let argTable = H.fromList $ updateArgTableRange (H.toList tempArgTable) varTable -- update the argTable with the computed sizes
	let output = gen_OpenCL_API_calls argTable argsNames consArgsNames srcLines [] -- generate the target source code
	write_F95_src gen_src_name output -- write generated source to file