module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where

data Line = ACCArgsBegin
          | ACCArgsEnd
          | ACCConstArgsBegin
          | ACCConstArgsEnds
          | CodeLine String

-- given the source code as a list of lines (strings), extract the OpenACC regions for Arguments and ConstArguments as well as the parameter declarations, and return them as a tuple of three lists of strings, in that order.
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = ([],[],[])
  
extract_Lines_from_F95_src :: [String] -> [Line]
extract_Lines_from_F95_src lines = map parseLine lines -- parseLines :: String -> Line

-- question: can regions have multiple levels?