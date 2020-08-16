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
    Ejecuta un parseo sobre una tira de caracteres (string) separandolas por espacios y los inserta consecutivamente 
    palabra por palabra en una lista
-}  


toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

{-
    Ejecuta un parseo sobre una tira de caracteres (string) separandolas por el carácter "_" y los inserta consecutivamente 
    en una lista. El propósito de esta función es agrupar correctamente conjunciones y preposiciones formadas por dos o más palabras.
-}  

toWords2 :: [Char] -> [[Char]] -- String -> [String]
toWords2 [] = []
toWords2 (x:xs) | x == '_'  = toWords2 (dropWhile ('_' ==) xs)
               | otherwise = (x:takeWhile ('_' /=) xs) : toWords2 (dropWhile ('_' /=) xs)


-- rotations recibe un título, como lista de palabras, y produce todas las rotaciones, sin importar si hay palabras no significativas
rotations xs = [ drop i xs ++ take i xs | i <- [0 .. n] ]
               where n = (length xs) - 1

-- Auxiliares para convertir palabras a minúsculas
lowercase = map toLower
lowercases = map lowercase

-- sigRotations recibe un título, como lista de palabras, y produce todas las rotaciones que comiencen con palabras significativas

sigRotations xs = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` noSignificativas) ]
                  where n = (length xs) - 1

-- titSigRotations es como sigRotations, pero pone primero el título de la obra, antes de todas las rotaciones significativas
titSigRotations xs = xs : [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` noSignificativas) ]
                   where n = (length xs) - 1

-- sep pone una secuencia de caracteres, "><", para indicar que los caracteres a la izquierda de ">" están
-- al final del título original, mientras que los caracteres a la derecha de ">" están al inicio del título original
sep xs = init xs ++ [last xs ++ " ><"]

kwic = nub . sort . concat . map pre
       where pre ys = map unwords (sigRotations (sep (toWords ys)))


kwicTitles = nub . sort . concat . map pre
             where pre ys = map unwords (titSigRotations (sep (toWords ys)))


-- Saca cada rotacion del Kwic y la imprime con un salto de linea 
printKwic ts = [putStrLn t | t <- kwic ts]

--printKwicTitles ts = map putStrLn (kwicTitles ts)

--main = sequence_ (printKwic titles) 


main :: IO ()
main = do

  putStrLn "***BIENVENIDO A KWIC Español *****"
  interactWithUser []
  putStrLn "Gracias por usar el programa. Se ha terminado la ejecucion"

type Item = String
type Items = [Item]

data Command
  = Quit
  | Kwic
  | Help

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["help"] -> Right Help
  ["kwic"] -> Right Kwic

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine           
  case parseCommand line of
    Right Help -> do
      putStrLn "Comandos: kwic, help, quit"
      interactWithUser items

    Right Quit -> do
      putStrLn "Ha elegido terminar la aplicacion"
      pure ()

    Right Kwic  -> do
      let rotaciones =  (printKwic titles)
      sequence_ rotaciones 
      interactWithUser items

    Left errMsg -> do
      putStrLn ("Ha habido un error: " ++ errMsg)
      interactWithUser items


-- PRUEBAS
-- Títulos, para probar
-- titles = ["As falls Wichita, so falls Wichita Falls", "The Yellow Submarine", "Kind of Blue", "The Mythical Man-Month"]
titles = ["El Descenso del Mal", "El Ascenso del Hombre", "El viajo y el Mar", "Un recuadro del artista joven", "El Submarino Amarillo"]


-- ************** Conjunto de Palabras no Significativas del Lenguaje Español *********

-- https://es.wikipedia.org/wiki/Preposición
-- http://www.auladiez.com/ejercicios/16_preposiciones.php
proposiciones = toWords2 "a_ante bajo_cabe_con_contra de_desde_durante_en_entre_hacia_hasta_mediante_para_por_según_sin_so_sobre_tras_versus_vía"

--https://englishlive.ef.com/es-mx/blog/laboratorio-de-gramatica/adjetivos-en-ingles/
--https://www.practicaespanol.com/wp-content/uploads/Adjetivos-listado-PDF.pdf
--https://www.aprendemasingles.com/2012/06/12/lista-de-60-adjetivos-comunes-con-traduccion-pdf/
adjetivos = toWords ("ácido enfadado dormido despierto malo precioso agrio brillante barato limpio claro frío fresco cruel profundo "++
            "delicado diferente sucio seco temprano agotado falso lejos gordo débil llano tonto estúpido libre lleno generoso bueno estupendo atractivo feliz duro saludable "++
            "pesado alto hueco caliente enorme hambriento enfermo izquierda ligero largo ruidoso bonito tacaño desordenado natural estrecho cerca necesario nuevo viejo abierto "++
            "contrario paralelo pobre privado rápido tranquilo preparado rico derecha correcto áspero triste seguro aserio filado corto cerrado simple fino  delicado suave sólido "++
            "especial picante empinado pegajoso recto  directo extraño fuerte repentino dulce grueso delgado ajustado ceñido cansado verdadero feo violento caluroso caliente débil "++
            "húmedo ancho sabio erróneo jóven")

--https://espanol.lingolia.com/es/gramatica/estructura-de-la-oracion/conjunciones
conjuciones = toWords2 "y_e_u_delande de_ni_no_solo_sino_tambien_tanto como_asi como_igual que_lo mismo que_pero_mas_empero_sino_mientras que_delante de_o bien_bien_ya sea_fuera"

--https://espanol.lingolia.com/es/gramatica/articulos
articulos = ["el","la","lo","los","las", "un", "unos", "una", "unas"]


--http://www.auladiez.com/fichas/47_adverbios.php
adverbios = toWords ("ahora antes después tarde luego ayer temprano ya todavía anteayer aún pronto hoy aquí ahí allí cerca lejos fuera dentro alrededor aparte"++
            "encima debajo delante detrás así bien mal despacio deprisa como mucho poco muy casi todo nada algo medio demasiado bastante más menos además"++
            "incluso también sí también asimismo no tampoco jamás nunca acaso quizá quizás tal vez")

--Fuente: https://www.ejemplos.co/50-ejemplos-de-pronombres/#ixzz6UGrJrQ6m
pronombres = toWords2 ("yo_tú_él_usted_ustedes_nosotros_nosotras_vosotros_vosotras_ellos_ellas_me_te_nos_se__mi_mis_mío_mía_míos_mías_tu_tus_tuyo_tuya_tuyos_"++
            "tuyas_su_sus_suyo_suya_suyos_suyas_nuestro_nuestra_nuestros_nuestras_vuestro_vuestra_vuestros_vuestras_este_ese_aquél_esta_esa_aquella_estos_"++
            "esos_aquellos_estas_esas_aquellas_esto_eso_aquello_quien_quienes_el cual_los cuales_la cual_las cuales_lo cual_cuyo_cuyos_cuyas_cuya_donde_qué_quién_"++
            "quiénes_cuál_cuáles_cuánto_cuántos_cuánta_cuántas_dónde_cómo_mucho_muchos_mucha_muchas_poco_pocos_poca_pocas_tanto_tantos_tanta_tantas_bastante_bastantes_"++
            "demasiado_demasiados_demasiada_demasiadas_alguno_algunos_alguna_algunas_ninguno_ninguna_algo_nada")


noSignificativas = proposiciones ++ adjetivos ++ conjuciones ++ articulos ++ adverbios ++ pronombres

