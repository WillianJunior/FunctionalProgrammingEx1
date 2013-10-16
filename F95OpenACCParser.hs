module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where
import Text.Regex.Posix -- suggest use of regular expressions
import Text.ParserCombinators.Parsec

type ArgumentsDeclarations = String
type ConstDeclarations = String
type ParametersDeclarations = String


extract_OpenACC_regions_from_F95_src :: [String] -> ([ArgumentsDeclarations],[ConstDeclarations],[ParametersDeclarations])
extract_OpenACC_regions_from_F95_src in_src_lines = ([],[],[])
  
f95_src :: GenParser Char st [[String]]
f95_src = 
	do
		result <- many (openACC_regions <|> otherCode)
		eof
		return result

openACC_regions :: GenParser Char st [[String]]
openACC_regions = 
	do
		return [[]] -- dummy

otherCode :: GenParser Char st [[String]]
otherCode = [[]]