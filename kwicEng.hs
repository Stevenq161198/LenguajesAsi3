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
    
    handleTitle <- openFile titlesFileName ReadMode
    contentTitle <- hGetContents handleTitle

    -- handleNSig <- readFile titlesFileName
    -- contentNSig <- hGetContents handleNSig
    putStrLn contentTitle
    hClose handleTitle

    handleOutput <- openFile outputFileName ReadWriteMode
    
    writeFile outputFileName "hola esto es una prueba"

    contentOut <- hGetContents handleOutput
    

    -- putStrLn $ "Trying to open " ++ titlesFilename ++ " " ++ ", to run the program"
    
    putStrLn $ "-------- output --------"
    -- putStrLn contentNSig
    putStrLn contentOut
    
    -- hClose handleNSig
    hClose handleOutput

--- 
-- main = do
--     handle <- openFile "file.txt" ReadWriteMode
--     contents <- hGetContents handle
--     writeFile "file2.txt" (map toUpper contents)
--     hClose handle
--     return ()
