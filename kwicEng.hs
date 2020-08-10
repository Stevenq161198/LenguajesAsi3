import Data.Char
import System.IO
import Control.Monad (when)


-- MENU --

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

    putStrLn $ "Trying to open " ++ titlesFilename ++ " " ++ ", to run the program"
    
    putStrLn $ "-------- output --------"

