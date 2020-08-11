import Data.Char
import System.IO
import Control.Monad (when)
import Data.List
type String = [Char]


toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

-- MENU --
algo = toWords "akjbsjybcjbai almvubks akhsbjybc hola"


main = do  
    putStrLn "What's the titles input file name?"
    titlesFileName <- getLine
    
    -- putStrLn "What's the notSignificants input file name?"  
    -- notSignificantsFileName <- getLine
    
    putStrLn "What's the output file name?"  
    outputFileName <- getLine

    handle <- openFile titlesFileName ReadWriteMode
    contents <- hGetContents handle
    writeFile outputFileName contents
    hClose handle

    putStrLn $ "-------- output --------"

