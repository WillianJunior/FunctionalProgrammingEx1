import System.Environment
import System.Exit

main = do
	args <- getArgs
	out <- parse args
	(putStr . tac) out

tac  = unlines . reverse . lines

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage   >> exit
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)