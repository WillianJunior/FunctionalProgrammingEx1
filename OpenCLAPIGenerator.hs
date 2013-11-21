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

 Since the code generation is pretty straightfoward the
 functions' comments are suposed to be enought.

 The strategy to generate the code is as folows:
 1 - classify the line using regular expressions in
 order to find the code regions
 2 - call the right code generation function using
 pattern maching
 3 - generate the code relative to the region
 4 - insert the generated code and keep iterating
 through the src file 

 -}

module OpenCLAPIGenerator (
	gen_OpenCL_API_calls
) where
import F95Types
import Text.Regex.Posix -- suggest use of regular expressions
import Data.Char
import qualified Data.Map as H (toList, lookup)

import System.Process -- only for localtime, entirely optional
import System.IO.Unsafe (unsafePerformIO) -- only for localtime, entirely optional

-- prefixes of generated code
-- prefix = ("!!! This code was generated on " ++ localtime ++ " from module_LES_ocl_TEMPL.f95 using ./generate_OpenCL_API_calls_from_ACC_pragmas.pl\n!!! DON'T EDIT !!! Edit module_LES_ocl_TEMPL.f95 instead, and regenerate.")
bufDeclsPref = "! OpenCL buffer declarations"
sizeDeclsPref = "\t! OpenCL buffer size declarations"
makeSizesPref = "\t\t! OpenCL buffer sizes"
makeBuffersPref = "\t\t! Create OpenCL buffers"
setArgsPref = "\t\t! Set OpenCL argument order"
writeBuffersPref = "\t\t! Copy all arrays required for the full run"


data ACCPragma = BufDecls
			   | SizeDecls
			   | MakeSizes
			   | MakeBuffers
			   | SetArgs
			   | WriteBuffers
			   | RemaningFlags -- used to remove the !$ACC... statements
			   | NotPragma String -- regular code
			   deriving (Show)

---------------------------------------------------------
-- generate the entire code

gen_OpenCL_API_calls :: ArgTable -> [String] -> [String] -> [String] -> String -> [String]    
gen_OpenCL_API_calls ocl_args arg_names const_arg_names src_lines templ_src_name = gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names $ map lineClassifier src_lines

gen_OpenCL_API_calls_helper :: ArgTable -> [String] -> [String] -> [ACCPragma] -> [String]  
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (BufDecls:pgs) = bufDeclsPref : (generateAllBufDecls arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (SizeDecls:pgs) = sizeDeclsPref : (generateAllSizeDecls ocl_args arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (MakeSizes:pgs) = makeSizesPref : (generateAllMakeSizes ocl_args arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (MakeBuffers:pgs) = makeBuffersPref : (generateAllMakeBuffers ocl_args arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (SetArgs:pgs) = setArgsPref : (generateAllSetArgs ocl_args arg_names const_arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (WriteBuffers:pgs) = writeBuffersPref : (generateAllWriteBuffers ocl_args arg_names) ++ gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names (RemaningFlags:pgs) = gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names ((NotPragma str):pgs) = str : gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names pgs
gen_OpenCL_API_calls_helper ocl_args arg_names const_arg_names [] = []

get_c_type :: VarType -> String
get_c_type vt = ""
                
ucfirst (x:xs)  = (toUpper x):xs

localtime = unsafePerformIO $ readProcess "/bin/date" [] []

-----------------------------------------------------------------------
-- classify a code line

lineClassifier :: String -> ACCPragma
lineClassifier str | (strLc =~ "^![/$]acc bufdecls([ ]*[\t]*)$" :: Bool) = BufDecls
				   | (strLc =~ "^![/$]acc sizedecls([ ]*[\t]*)$" :: Bool) = SizeDecls
				   | (strLc =~ "^![/$]acc makesizes([ ]*[\t]*)$" :: Bool) = MakeSizes
				   | (strLc =~ "^![/$]acc makebuffers([ ]*[\t]*)$" :: Bool) = MakeBuffers
				   | (strLc =~ "^![/$]acc setargs([ ]*[\t]*)$" :: Bool) = SetArgs
				   | (strLc =~ "^![/$]acc writebuffers([ ]*[\t]*)$" :: Bool) = WriteBuffers
				   | (strLc =~ "^![/$]acc" :: Bool) = RemaningFlags
				   | otherwise = NotPragma str
	where
		strLc = map toLower str

------------------------------------------------------------------
-- code generation functions

generateAllBufDecls :: [String] -> [String]
generateAllBufDecls = map (\str -> ("\t\tinteger(8) :: " ++ str ++ "_buf"))

generateAllSizeDecls :: ArgTable -> [String] -> [String]
generateAllSizeDecls ocl_args arg_names = [("\t\tinteger, dimension(" ++ dim ++ "):: " ++ arg_name ++ "_sz") | 
			let args = H.toList ocl_args, 
			(dim, arg) <- join2 (map (show.length.vd_dimension.snd) args) args, 
			arg_name <- arg_names, 
			elem arg_name $ (vd_varlist.snd) arg]

generateAllMakeSizes :: ArgTable -> [String] -> [String]
generateAllMakeSizes ocl_args (name:names) 
		| arg == Nothing = generateAllMakeSizes ocl_args names
		| calculatedDimension /= Nothing = [("\t\t" ++ name ++"_sz = (/ " ++ (numbersList $ just calculatedDimension) ++ " /)")] ++ generateAllMakeSizes ocl_args names
		| otherwise = ["\t\t" ++ name ++ "_sz = shape(" ++ name ++ ")"] ++ generateAllMakeSizes ocl_args names
	where
		arg = H.lookup name ocl_args
		calculatedDimension = calculateDim.vd_dimension $ just arg
generateAllMakeSizes _ [] = []

generateAllMakeBuffers :: ArgTable -> [String] -> [String]
generateAllMakeBuffers ocl_args arg_names = [("\t\tcall oclMake" ++ dim ++ "D" ++ var_type ++ "Array" ++ arg_mode ++ "Buffer(" ++ arg_name ++ "_buf," ++ arg_name ++ "_sz," ++ arg_name ++ ")") |
			let args = H.toList ocl_args, 
			(dim, var_type, arg_mode, arg) <- join4 (map (show.length.vd_dimension.snd) args) (map (show.at_numtype.vd_vartype.snd) args) (map (show.vd_argmode.snd) args) args, 
			arg_name <- arg_names,
			elem arg_name $ (vd_varlist.snd) arg]

generateAllSetArgs :: ArgTable -> [String] -> [String] -> [String]
generateAllSetArgs ocl_args arg_names const_arg_names = (generateAllSetArgsArrays ocl_args arg_names) ++ (generateAllSetArgsConsts ocl_args const_arg_names (length arg_names))

generateAllSetArgsArrays :: ArgTable -> [String] -> [String]
generateAllSetArgsArrays ocl_args arg_names = enumerate 0 [("\t\tcall oclSet" ++ var_type ++ "ArrayArg(", ", " ++ arg_name ++ "_buf )") |
			let args = H.toList ocl_args, 
			(var_type, arg) <- join2 (map (show.at_numtype.vd_vartype.snd) args) args, 
			arg_name <- arg_names, 
			elem arg_name $ (vd_varlist.snd) arg]

generateAllSetArgsConsts :: ArgTable -> [String] -> Int -> [String]
generateAllSetArgsConsts ocl_args const_arg_names startNum = enumerate startNum [("\t\tcall oclSet" ++ var_type ++ "ConstArg(", ", " ++ arg_name ++ " )") |
			let args = H.toList ocl_args, 
			(var_type, arg) <- join2 (map (show.at_numtype.vd_vartype.snd) args) args, 
			arg_name <- const_arg_names, 
			elem arg_name $ (vd_varlist.snd) arg]

generateAllWriteBuffers :: ArgTable -> [String] -> [String]
generateAllWriteBuffers ocl_args arg_names = [("\t\tcall oclWrite" ++ dim ++ "D" ++ var_type ++ "ArrayBuffer(" ++ arg_name ++ "_buf, " ++ arg_name ++ "_sz, " ++ arg_name ++ " )") | 
			let args = H.toList ocl_args, 
			(dim, var_type, arg) <- join3 (map (show.length.vd_dimension.snd) args) (map (show.at_numtype.vd_vartype.snd) args) args, 
			arg_name <- arg_names,
			elem arg_name $ (vd_varlist.snd) arg]

------------------------------------------------------------------
-- helper functions

join2 :: [a] -> [b] -> [(a,b)]
join2 (x:xs) (y:ys) = (x,y) : join2 xs ys
join2 [] [] = []

join3 :: [a] -> [b] -> [c] -> [(a,b,c)]
join3 (x:xs) (y:ys) (z:zs) = (x,y,z) : join3 xs ys zs
join3 [] [] [] = []

join4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
join4 (w:ws) (x:xs) (y:ys) (z:zs) = (w,x,y,z) : join4 ws xs ys zs
join4 [] [] [] [] = []

enumerate :: Int -> [(String, String)] -> [String]
enumerate current ((a,b):s) = (a ++ (show current) ++ b) : enumerate (current+1) s
enumerate _ [] = []

numbersList :: [Integer] -> String
numbersList [x] = show x
numbersList (x:xs) = (show x) ++ ", " ++ numbersList xs

calculateDim :: [Range] -> Maybe [Integer]
calculateDim = flatDim.calculateDimHelper

flatDim :: [Maybe Integer] -> Maybe [Integer]
flatDim rs = if elem Nothing rs then Nothing else Just (map just rs)

just :: Maybe a -> a
just (Just a) = a

calculateDimHelper :: [Range] -> [Maybe Integer]
calculateDimHelper = map calculateRange

calculateRange :: Range -> Maybe Integer
calculateRange (MkRange (Const start) (Const end)) = Just (end - start)
calculateRange _ = Nothing
