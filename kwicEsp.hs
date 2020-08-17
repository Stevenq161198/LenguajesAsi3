{-
    Implementación Kwic Básico Español
    - ESTUDIANTES:
        1. Danny Andrés Piedra Acuña   
        2. Steven Quesada Jimenez
    - Curso: Lenguajes de Programación
    - Fecha de Entrega: 17-08-2020
-}

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

--Librerias para el correcto funcionamiento de nuestro programa
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import Control.Monad (when)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.HashSet as HashSet hiding(map, sort)
import System.Directory (doesFileExist)

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

check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $
    s ++
    if result
      then " el archivo ya existe"
      else " el archivo no existe"

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

    let outputFile = check doesFileExist outputFileName
    outputFile
    let outputFile2 = check doesFileExist outputFileName2
    outputFile2

    writeFile outputFileName (intercalate "\n" kwicBasic)
    writeFile outputFileName2 (intercalate "\n" kwicAling)


-- ********** Conjuntos de palabras ********** --
-- Estan en el archivo .txt, esta es solo un ejemplo de que hay en ese otro archivo:

-- Títulos, para probar
titles = ["El Descenso del Mal", "El Ascenso del Hombre", "El viajo y el Mar", "Un recuadro del artista joven", "El Submarino Amarillo"]


proposiciones = "a_ante bajo_cabe_con_contra de_desde_durante_en_entre_hacia_hasta_mediante_para_por_según_sin_so_sobre_tras_versus_vía"


adjetivos =  ("ácido enfadado dormido despierto malo precioso agrio brillante barato limpio claro frío fresco cruel profundo "++
            "delicado diferente sucio seco temprano agotado falso lejos gordo débil llano tonto estúpido libre lleno generoso bueno estupendo atractivo feliz duro saludable "++
            "pesado alto hueco caliente enorme hambriento enfermo izquierda ligero largo ruidoso bonito tacaño desordenado natural estrecho cerca necesario nuevo viejo abierto "++
            "contrario paralelo pobre privado rápido tranquilo preparado rico derecha correcto áspero triste seguro aserio filado corto cerrado simple fino  delicado suave sólido "++
            "especial picante empinado pegajoso recto  directo extraño fuerte repentino dulce grueso delgado ajustado ceñido cansado verdadero feo violento caluroso caliente débil "++
            "húmedo ancho sabio erróneo jóven")

conjuciones =  "y_e_u_delande de_ni_no_solo_sino_tambien_tanto como_asi como_igual que_lo mismo que_pero_mas_empero_sino_mientras que_delante de_o bien_bien_ya sea_fuera"


articulos = ["el","la","lo","los","las", "un", "unos", "una", "unas"]


  

adverbios = ("ahora antes después tarde luego ayer temprano ya todavía anteayer aún pronto hoy aquí ahí allí cerca lejos fuera dentro alrededor aparte"++
            "encima debajo delante detrás así bien mal despacio deprisa como mucho poco muy casi todo nada algo medio demasiado bastante más menos además"++
            "incluso también sí también asimismo no tampoco jamás nunca acaso quizá quizás tal vez")

--Fuente: 
pronombres = ("yo_tú_él_usted_ustedes_nosotros_nosotras_vosotros_vosotras_ellos_ellas_me_te_nos_se__mi_mis_mío_mía_míos_mías_tu_tus_tuyo_tuya_tuyos_"++
            "tuyas_su_sus_suyo_suya_suyos_suyas_nuestro_nuestra_nuestros_nuestras_vuestro_vuestra_vuestros_vuestras_este_ese_aquél_esta_esa_aquella_estos_"++
            "esos_aquellos_estas_esas_aquellas_esto_eso_aquello_quien_quienes_el cual_los cuales_la cual_las cuales_lo cual_cuyo_cuyos_cuyas_cuya_donde_qué_quién_"++
            "quiénes_cuál_cuáles_cuánto_cuántos_cuánta_cuántas_dónde_cómo_mucho_muchos_mucha_muchas_poco_pocos_poca_pocas_tanto_tantos_tanta_tantas_bastante_bastantes_"++
            "demasiado_demasiados_demasiada_demasiadas_alguno_algunos_alguna_algunas_ninguno_ninguna_algo_nada")
