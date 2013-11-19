module F95OpenACCParser where
    --extract_OpenACC_regions_from_F95_src
--) where

import Text.Regex.Posix

type Stack = [Line]

data Line = ACCArgsBegin
          | ACCArgsEnd
          | ACCConstArgsBegin
          | ACCConstArgsEnds
          | CodeLine String
          deriving (Show)


data LineType = Arg String
			  | ConstArg String
			  | Param String
			  deriving (Show)

-- given the source code as a list of lines (strings), extract the OpenACC regions for Arguments and ConstArguments as well as the parameter declarations, and return them as a tuple of three lists of strings, in that order.
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = (args, consts, params)
	where	
		args = extract_Arg lines
		consts = extract_Const lines
		params = extract_Params in_src_lines
		lines = line_classifier (extract_Limits in_src_lines) []

line_classifier :: [Line] -> Stack -> [LineType]
line_classifier (ACCArgsBegin:xs) st = line_classifier xs (ACCArgsBegin:st)
line_classifier (ACCArgsEnd:xs) (st:sts) = line_classifier xs sts
line_classifier (ACCConstArgsBegin:xs) st = line_classifier xs (ACCConstArgsBegin:st)
line_classifier (ACCConstArgsEnds:xs) (st:sts) = line_classifier xs sts
line_classifier (CodeLine x:xs) st@(ACCArgsBegin:_) = (Arg x) : line_classifier xs st
line_classifier (CodeLine x:xs) st@(ACCConstArgsBegin:_) = (ConstArg x) : line_classifier xs st
line_classifier (CodeLine x:xs) [] = line_classifier xs []
line_classifier [] [] = []

extract_Arg :: [LineType] -> [String]
extract_Arg ((Arg x):xs) = x : extract_Arg xs
extract_Arg (x:xs) = extract_Arg xs
extract_Arg [] = []

extract_Const :: [LineType] -> [String]
extract_Const ((ConstArg x):xs) = x : extract_Const xs
extract_Const (x:xs) = extract_Const xs
extract_Const [] = []

extract_Params :: [String] -> [String]
extract_Params (str:xs) | (str =~ "!" :: Bool) = extract_Params xs
						| (str =~ "parameter" :: Bool) = str : extract_Params xs
						| otherwise = extract_Params xs
extract_Params [] = []

extract_Limits :: [String] -> [Line]
extract_Limits (str:xs) | (str =~ "^![/$]acc arguments([ ]*[\t]*)$" :: Bool) = ACCArgsBegin : extract_Limits xs	
						| (str =~ "^![/$]acc end arguments([ ]*[\t]*)$" :: Bool) = ACCArgsEnd : extract_Limits xs
						| (str =~ "^![/$]acc constarguments([ ]*[\t]*)$" :: Bool) = ACCConstArgsBegin : extract_Limits xs
						| (str =~ "^![/$]acc end constarguments([ ]*[\t]*)$" :: Bool) = ACCConstArgsEnds : extract_Limits xs
						| otherwise = CodeLine str : extract_Limits xs
extract_Limits [] = []