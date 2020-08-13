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



-- Agrupar las palabras presentes en el título de una obra bibliográfica
-- Notar que solo se desechan los caracteres en blanco.
-- Los signos de puntuación se mantienen a la derecha de la palabra que les precede, si no hay espacios entre esos caracteres.

toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == '_'  = toWords (dropWhile ('_' ==) xs)
               | otherwise = (x:takeWhile ('_' /=) xs) : toWords (dropWhile ('_' /=) xs)


-- ************** Conjunto de Palabras no Significativas del Lenguaje Español *********

-- https://es.wikipedia.org/wiki/Preposición
-- http://www.auladiez.com/ejercicios/16_preposiciones.php
proposiciones = "a_ante bajo_cabe_con_contra de_desde_durante_en_entre_hacia_hasta_mediante_para_por_según_sin_so_sobre_tras_versus_vía"

-- texto sin tildes para haskell pueda hacer un print bonito
conjuciones = "y_e_u_delande de_ni_no_solo_sino_tambien_tanto como_asi como_igual que_lo mismo que_pero_mas_empero_sino_mientras que_delante de_o bien_bien_ya sea_fuera"


--https://englishlive.ef.com/es-mx/blog/laboratorio-de-gramatica/adjetivos-en-ingles/
--https://www.practicaespanol.com/wp-content/uploads/Adjetivos-listado-PDF.pdf
--https://www.aprendemasingles.com/2012/06/12/lista-de-60-adjetivos-comunes-con-traduccion-pdf/
adjetivos ="ácido enfadado dormido despierto malo precioso agrio brillante barato limpio claro frío fresco cruel profundo 
            delicado diferente sucio seco temprano agotado falso lejos gordo débil llano tonto estúpido libre lleno generoso bueno estupendo atractivo feliz duro saludable 
            pesado alto hueco caliente enorme hambriento enfermo izquierda ligero largo ruidoso bonito tacaño desordenado natural estrecho cerca necesario nuevo viejo abierto
            contrario paralelo pobre privado rápido tranquilo preparado rico derecha correcto áspero triste seguro aserio filado corto cerrado simple fino  delicado suave sólido
            especial picante empinado pegajoso recto  directo extraño fuerte repentino dulce grueso delgado ajustado ceñido cansado verdadero feo violento caluroso caliente débil
            húmedo ancho sabio erróneo jóven"




