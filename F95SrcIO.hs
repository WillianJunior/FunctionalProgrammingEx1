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
 was tested is working.
 
 -}

module F95SrcIO (
		read_F95_src,
        write_F95_src
) where
import Data.Char (toLower)
import System.IO
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower

read_F95_src :: String -> IO [String]
read_F95_src src_name = do  
	handle <- openFile src_name ReadMode  
	contents <- hGetContents handle  
	return $ lines contents

write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines = do  
	handler <- openFile src_name WriteMode
	printLinesToHandler handler src_lines

printLinesToHandler :: Handle -> [String] -> IO ()
printLinesToHandler _ [] = return ()
printLinesToHandler handler (x:xs) = do
	hPutStrLn handler x
	printLinesToHandler handler xs