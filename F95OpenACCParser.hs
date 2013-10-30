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
need_a_better_name (ACCArgsBegin:xs) st = need_a_better_name xs (ACCArgsBegin:st)
need_a_better_name (ACCArgsEnd:xs) (st:sts) = need_a_better_name xs sts
need_a_better_name (ACCConstArgsBegin:xs) st = need_a_better_name xs (ACCConstArgsBegin:st)
need_a_better_name (ACCConstArgsEnds:xs) (st:sts) = need_a_better_name xs sts
-- need_a_better_name (ACCParamBegin:xs) st = need_a_better_name xs (ACCParamBegin:st)
need_a_better_name (ACCParamBegin:xs) st = need_a_better_name xs st -- temp: ignore the paramBeg flag
need_a_better_name (CodeLine x:xs) st@(ACCArgsBegin:_) = (Arg x) : need_a_better_name xs st
need_a_better_name (CodeLine x:xs) st@(ACCConstArgsBegin:_) = (ConstArg x) : need_a_better_name xs st
need_a_better_name (CodeLine x:xs) st@(ACCParamBegin:_) = (Param x) : need_a_better_name xs st
need_a_better_name (CodeLine x:xs) [] = need_a_better_name xs []
need_a_better_name [] _ = []

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