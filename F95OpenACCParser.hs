module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where

import OpenACCRegions

type Stack = [Line]
data LineType = Arg String
			  | ConstArg String
			  | Param String
			  deriving (Show)

-- given the source code as a list of lines (strings), extract the OpenACC regions for Arguments and ConstArguments as well as the parameter declarations, and return them as a tuple of three lists of strings, in that order.
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = ([],[],[])
  
extract_Lines_from_F95_src :: [String] -> [Line]
extract_Lines_from_F95_src lines = foldr (++) [] $ map scanACCRegions lines

need_a_better_name :: [Line] -> Stack -> [LineType]
need_a_better_name (ACCArgsBegin:xs) st reg = need_a_better_name xs (ACCArgsBegin:st) reg

-- question: can regions have multiple levels?
-- question: where does the param begins and ends?
-- question: this version ignore empty new line, is that a problem?

--data Line = ACCArgsBegin
--          | ACCArgsEnd
--          | ACCConstArgsBegin
--          | ACCConstArgsEnds
--          | ACCParamBegin
--          | CodeLine String
--          deriving (Show)