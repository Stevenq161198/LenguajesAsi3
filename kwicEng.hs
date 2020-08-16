{- 
    - Curso: Lenguajes de Programación
    - Fecha de Entrega: 17-08-2020
    
         1. Danny Piedra Acuña
         2. Steven Quesada Jimenez
    - Curso: Lenguajes de Programación
    - Fecha de Entrega: 17-08-2020
-}

{- 
Referencias:
  https://www.cs.cmu.edu/~ModProb/KWIC.html
  https://www.librarianshipstudies.com/2017/02/keyword-in-context-kwic-indexing.html
  https://en.wikipedia.org/wiki/Key_Word_in_Context
  https://gitlab.univ-nantes.fr/cohen-j/haskell-view-switcher/-/wikis/kwic/history
  https://www.sketchengine.eu/my_keywords/kwic/
-}

import Data.Char
import Data.List
import Data.List.Split
import System.IO
import Control.Monad (when)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.HashSet as HashSet hiding(map, sort)

type HashT = HashSet String
-- ************** Conjunto de Palabras no Significativas del Lenguaje Inglés *********

-- lista de palabras no significativas, se unen todas las listas en una sola

notSignificants :: [Char] -> HashT
notSignificants nsig= HashSet.fromList(splitOn "," (lowercase nsig))


--Una version mejorada de toWords, convierte en minuscula, separada por salto de linea 
--y devuelve una lista de listas de titulos
lowTitles :: [Char] -> [[[Char]]]
lowTitles ntit = map (splitOn " " )(splitOn "\n" (lowercase ntit))


-- Auxiliares para convertir palabras a minúsculas
lowercase = map toLower 
lowercases = map lowercase

-- sigRotations recibe un título, como lista de palabras, y produce todas las rotaciones que comiencen con palabras significativas
sigRotations ::  HashT -> [[Char]] -> [[[Char]]]
sigRotations xr xs = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` xr) ]
                  where n = (length xs) - 1


-- putSpaces coloca un espacio en blanco entre las palabras presentes en una lista de palabras, y devuelve una hilera
putSpaces xss = tail (concat (map (' ':) xss))


-- sep pone "><" colocando a la derecha los caracteres que estan al inicio del titulo y a la izquierda los del final
sep xs = init xs ++ [last xs ++ " ><"]

funBonita :: [Char] -> [Char]
funBonita untitulo = do
  let a = splitOn "><" untitulo
  
  let b = splitOn " " (a !! 0)

  let c = map toUpper (b !! 0)

  (a !! 1) ++ " " ++ c ++ " " ++ intercalate " "(tail b)

--Ajusta cada título insertando la cantidad de espacios necesarios, esto lo hace restandole
--el largo de cada uno al largo del mayor título, para saber donde insertar cada uno.

alignOn :: [String] -> [String]
alignOn lines = map padline lines
  where
    partBeforechar = head . splitWhen isUpper
    longestLengthBeforeChar = maximum $ map (length . partBeforechar) lines
    padline line = replicate offset ' ' ++ line
      where
        offset = longestLengthBeforeChar - (length (partBeforechar line))


--Calcula el "peor caso", es decir, el título de mayor tamaño en una lista de titulos

longestStrings :: [String] -> [String]
longestStrings = go [] 0
  where
  go acc _ []       = acc  -- base case
  go acc n (x:xs)
    | length x > n  = go [x] (length x) xs
    | length x == n = go (x:acc) n xs
    | otherwise     = go acc n xs


--Toma los titulos, los inserta en una estructura de datos Hash, le calcula las rotaciones a los titulos
--y los separa por espacios.

kwic :: HashSet String -> [[Char]] -> [[Char]]
kwic hash titulos = do 
    let sigs = map putSpaces (sigRotations hash(sep titulos))
    let kwiwFormat = (nub sigs)
    kwiwFormat


main :: IO ()
main = do
    putStrLn "Cuál es el nombre del archivo de Títulos?"
    titlesFileName <- getLine
    
    putStrLn "Cuá es el nombre del archivo de palabras no Significativas en Inglés?"  
    notSignificantsFileName <- getLine

    putStrLn "Cuál es el nombre del archivo de salida?"
    outputFileName <- getLine

    putStrLn "Cuál es el nombre del 2do archivo de salida?"
    outputFileName2 <- getLine

    contentT <- readFile titlesFileName
    let titles = lowTitles contentT

    contentS <- readFile notSignificantsFileName
    let notSig = notSignificants contentS

    let kwicBasic = concat(map(kwic notSig) titles)

    let kwicAling = alignOn(map funBonita(concat(map(kwic notSig) titles)))

    writeFile outputFileName (intercalate "\n" kwicBasic)
    writeFile outputFileName2 (intercalate "\n" kwicAling)
      

