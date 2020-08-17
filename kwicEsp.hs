{-
    Implementación Kwic Básico Español
    - ESTUDIANTES:
        1. Danny Andrés Piedra Acuña   
        2. Steven Quesada Jimenez
    - Curso: Lenguajes de Programación
    - Fecha de Entrega: 17-08-2020
-}

--Librerias para el correcto funcionamiento de nuestro programa
import Data.Char
import Data.List
import System.IO
import Control.Monad (when)
import qualified Data.Text    as T
import qualified Data.Text.IO as T


{- 
Referencias:
  http://www.auladiez.com/fichas/47_adverbios.php
  https://www.ejemplos.co/50-ejemplos-de-pronombres/#ixzz6UGrJrQ6m
  https://espanol.lingolia.com/es/gramatica/articulos
  https://espanol.lingolia.com/es/gramatica/estructura-de-la-oracion/conjunciones
  https://www.practicaespanol.com/wp-content/uploads/Adjetivos-listado-PDF.pdf
  https://www.aprendemasingles.com/2012/06/12/lista-de-60-adjetivos-comunes-con-traduccion-pdf
  https://es.wikipedia.org/wiki/Preposición
  http://www.auladiez.com/ejercicios/16_preposiciones.php
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
-- ************** Conjunto de Palabras no Significativas del Lenguaje Español *********

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


-- putSpaces coloca un espacio en blanco entre las palabras presentes en una 
-- lista de palabras, y devuelve una hilera
putSpaces xss = tail (concat (map (' ':) xss))


-- sep pone "><" colocando a la derecha los caracteres que estan al inicio del titulo y a la izquierda los del final
sep xs = init xs ++ [last xs ++ " ><"]

--Funcion que imprime centrando los titulos.
--Separa un titulo donde empieza la palabra significativa. Le agrega un espacio y la convierte a mayuscula.

funBonita :: [Char] -> [Char]
funBonita untitulo = do
  let word = splitOn "><" untitulo
  
  let adjustedWord = splitOn " " (word !! 0)

  let upperCaseWord = map toUpper (adjustedWord !! 0)

  (word !! 1) ++ " " ++ upperCaseWord ++ " " ++ intercalate " "(tail adjustedWord)


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


--Toma los titulos, los inserta en una estructura de datos Hash, le calcula las rotaciones a los titulos
--y los separa por espacios.
kwic :: HashSet String -> [[Char]] -> [[Char]]
kwic hash titulos = do 
    let sigs = map putSpaces (sigRotations hash(sep titulos))
    let formatoKwic = (nub sigs)
    formatoKwic


main :: IO ()
main = do
    putStrLn "Cuál es el nombre del archivo de Títulos?"
    titlesFileName <- getLine
    
    putStrLn "Cuál es el nombre del archivo de palabras no Significativas en Español?"  
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
