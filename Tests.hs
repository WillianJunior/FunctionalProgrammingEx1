import System.IO

getLinesFromFile :: String -> IO [String]
getLinesFromFile path = do  
	handle <- openFile path ReadMode  
	contents <- hGetContents handle  
	return $ lines contents