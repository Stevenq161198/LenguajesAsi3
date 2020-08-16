{-
    Lectura correcta de archivos de entrada y Escritura correcta del archivo de salida
    - ESTUDIANTES:
        1. Danny Andrés Piedra Acuña   
        2. Steven Quesada Jimenez
    - Curso: Lenguajes de Programación
    - Fecha de Entrega: 17-08-2020
-}

--Librerías necesarias para el correcto funcionamiento del Programa
import Data.Char
import System.IO
import Control.Monad (when)
import Data.List
type String = [Char]

--Programa que maneja la entrada y salida de archivos
toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)
main = do
    putStrLn "Cuál es el nombre del archivo de Títulos?"
    titlesFileName <- getLine
    
    -- putStrLn "Cuá es el nombre del archivo de palabras no Significativas en Inglés?"  
    -- notSignificantsFileName <- getLine
    
    putStrLn "Cuál es el nombre del archivo de salida?"
    outputFileName <- getLine

    contents <- readFile titlesFileName
    let h = toWords contents
    writeFile outputFileName (intercalate "\n" h)