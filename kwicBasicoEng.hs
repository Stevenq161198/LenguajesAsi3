-- Implementación Kwic Básico Inglés
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
import System.IO
import Control.Monad (when)

-- Agrupar las palabras presentes en el título de una obra bibliográfica
-- Notar que solo se desechan los caracteres en blanco.
-- Los signos de puntuación se mantienen a la derecha de la palabra que les precede, si no hay espacios entre esos caracteres.

toWords :: [Char] -> [[Char]] -- String -> [String]
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

-- ************** Conjunto de Palabras no Significativas del Lenguaje Inglés *********

{-
Articles:
  https://www.grammarly.com/blog/articles/
-}               
articles = ["a", "an", "some", "the"]

{- 
Prepositions:
  https://www.englishclub.com/grammar/prepositions-list.htm
-}
prepositions = toWords "aboard about above across after against along amid among anti around as at before behind below beneath beside besides between beyond but by concerning considering despite down during except excepting excluding following for from in inside into like minus near of off on onto opposite outside over past per plus regarding round save since than through to toward towards under underneath unlike until up upon versus via with within without"

{- 
Adjectives:
  https://grammar.yourdictionary.com/parts-of-speech/adjectives/list-of-adjective-words.html
  https://www.paperrater.com/page/lists-of-adjectives
  https://www.talkenglish.com/vocabulary/top-500-adjectives.aspx
-}
adjectives = toWords ("attractive bald beautiful chubby clean dazzling drab elegant fancy fit flabby glamorous gorgeous handsome long magnificent muscular plain plump quaint scruffy shapely short skinny stocky ugly unkempt unsightly ashy black blue gray green icy lemon mango orange purple red salmon white yellow alive better careful clever dead easy famous gifted hallowed helpful important inexpensive mealy mushy odd poor powerful rich shy tender unimportant "++ 
                      "uninterested vast wrong aggressive agreeable ambitious brave calm delightful eager faithful gentle happy jolly kind lively nice obedient polite proud silly thankful victorious witty wonderful zealous angry bewildered clumsy defeated embarrassed fierce grumpy helpless itchy jealous lazy mysterious nervous obnoxious panicky pitiful repulsive scary thoughtless uptight worried broad chubby crooked curved deep flat high hollow low narrow refined round shallow "++ 
                      "skinny square steep straight wide big colossal fat gigantic great huge immense large little mammoth massive microscopic miniature petite puny scrawny short small tall teeny tiny crashing deafening echoing faint harsh hissing howling loud melodic noisy purring quiet rapping raspy rhythmic screeching shrilling squeaking thundering tinkling wailing whining whispering ancient brief early fast future late long modern old old-fashioned prehistoric quick rapid "++ 
                      "short slow swift young acidic bitter cool creamy delicious disgusting fresh greasy juicy hot moldy nutritious nutty putrid rancid ripe rotten salty savory sour spicy spoiled stale sweet tangy tart tasteless tasty yummy breezy bumpy chilly cold cool cuddly damaged damp dirty dry flaky fluffy freezing greasy hot icy loose melted prickly rough shaggy sharp slimy sticky strong tight uneven warm weak wet wooden abundant billions enough few full hundreds incalculable "++ 
                      "limited little many most millions numerous scarce some sparse substantial thousands")

{-
Adverbs:
  https://www.enchantedlearning.com/wordlist/adverbs.shtml
  https://grammar.yourdictionary.com/parts-of-speech/adverbs/list-of-100-adverbs.html
  https://7esl.com/list-of-adverbs/
  https://www.talkenglish.com/vocabulary/top-250-adverbs.aspx
  https://usefulenglish.ru/writing/list-of-adverbs
-}

adverbs = toWords "generally generously gently genuinely girlishly gladly gleefully gracefully graciously gradually gratefully greatly greedily grimly grudgingly"

{-
Pronouns:
  https://www.thefreedictionary.com/List-of-pronouns.htm
  https://www.english-grammar-revolution.com/list-of-pronouns.html
  https://www.really-learn-english.com/list-of-pronouns.html
  https://www.ef.com/ca/english-resources/english-grammar/pronouns/
  https://www.englishclub.com/vocabulary/pronouns-list.php
  https://www.englishclub.com/vocabulary/pronouns-type.php
-}
pronouns = toWords "all another any anybody anyone anything aught both each each either enough everybody everyone everything few he her hers herself him himself his I it itself many me mine most myself naught neither no nobody none nothing nought one other others ought ours ourself ourselves several she some somebody someone something such suchlike that thee theirs theirself theirselves them themself themselves there these they thine this those thou thy thyself us we what whatever whatsoever whether which whichever whichsoever who whoever whom whomever whomso whomsoever whose whosever whosesoever whoso whosoever ye yon you yours yourself yourselves"


{-
Conjunctions:
  https://www.englishclub.com/grammar/conjunctions.htm
  https://www.grammarly.com/blog/conjunctions/
  https://dictionary.cambridge.org/grammar/british-grammar/conjunctions
  https://www.smart-words.org/linking-words/conjunctions.html
-}

conjunctions = toWords "after also although and as because before but either for how if neither nor once only or since so than that though till until when where whereas whether while yet"

-- la lista de palabras no significativas en los títulos puede parecer arbitraria, pero es conveniente 
notSignificants = articles ++ prepositions ++ adjectives ++ adverbs ++ pronouns ++ conjunctions

-- rotations recibe un título, como lista de palabras, y produce todas las rotaciones, sin importar si hay palabras no significativas
rotations xs = [ drop i xs ++ take i xs | i <- [0 .. n] ]
               where n = (length xs) - 1

-- Auxiliares para convertir palabras a minúsculas
lowercase = map toLower 
lowercases = map lowercase

-- sigRotations recibe un título, como lista de palabras, y produce todas las rotaciones que comiencen con palabras significativas
sigRotations xs = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                  where n = (length xs) - 1

-- titSigRotations es como sigRotations, pero pone primero el título de la obra, antes de todas las rotaciones significativas
titSigRotations xs = xs : [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                   where n = (length xs) - 1

-- putSpaces coloca un espacio en blanco entre las palabras presentes en una lista de palabras, y devuelve una hilera
putSpaces xss = tail (concat (map (' ':) xss))

-- sep pone una secuencia de caracteres, "><", para indicar que los caracteres a la izquierda de ">" están
-- al final del título original, mientras que los caracteres a la derecha de ">" están al inicio del título original
sep xs = init xs ++ [last xs ++ " ><"]

kwic = nub . sort . concat . map pre
       where pre ys = map putSpaces (sigRotations (sep (toWords ys)))

kwicTitles = nub . sort . concat . map pre
             where pre ys = map putSpaces (titSigRotations (sep (toWords ys)))


--Toma linea por linea e imprime cada indice del Kwic
printKwic ts = map putStrLn (kwic ts)

printKwicTitles ts = map putStrLn (kwicTitles ts)

-- ********   PRUEBAS *****
-- titles = ["As falls Wichita, so falls Wichita Falls", "The Yellow Submarine", "Kind of Blue", "The Mythical Man-Month"]
titles = ["Descent of Man", "The Ascent of Man", "The Old Man and The Sea", "A Portrait of The Artist As a Young Man"]



main :: IO ()
main = do

  putStrLn "***BIENVENIDO A KWIC Ingles *****"
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
      let rotations = printKwic titles
      sequence_ rotations
      interactWithUser items

    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items
