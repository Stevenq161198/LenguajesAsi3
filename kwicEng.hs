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
import System.Directory (doesFileExist)

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
    let kwiwFormat = (nub sigs)
    kwiwFormat




main :: IO ()
main = do
    putStrLn "Cuál es el nombre del archivo de Títulos?"
    titlesFileName <- getLine
    
    putStrLn "Cuál es el nombre del archivo de palabras no Significativas en Inglés?"  
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

{-
Articles:
  https://www.grammarly.com/blog/articles/
-}               
articles = ["a", "an", "some", "the"]

{- 
Prepositions:
  https://www.englishclub.com/grammar/prepositions-list.htm
-}
prepositions = "aboard about above across after against along amid among anti around as at before behind below beneath beside besides between beyond but by concerning considering despite down during except excepting excluding following for from in inside into like minus near of off on onto opposite outside over past per plus regarding round save since than through to toward towards under underneath unlike until up upon versus via with within without"

{- 
Adjectives:
  https://grammar.yourdictionary.com/parts-of-speech/adjectives/list-of-adjective-words.html
  https://www.paperrater.com/page/lists-of-adjectives
  https://www.talkenglish.com/vocabulary/top-500-adjectives.aspx
-}
adjectives = "attractive bald beautiful chubby clean dazzling drab elegant fancy fit flabby glamorous gorgeous handsome long magnificent muscular plain plump quaint scruffy shapely short skinny stocky ugly unkempt unsightly ashy black blue gray green icy lemon mango orange purple red salmon white yellow alive better careful clever dead easy famous gifted hallowed helpful important inexpensive mealy mushy odd poor powerful rich shy tender unimportant uninterested vast wrong aggressive agreeable ambitious brave calm delightful eager faithful gentle happy jolly kind lively nice obedient polite proud silly thankful victorious witty wonderful zealous angry bewildered clumsy defeated embarrassed fierce grumpy helpless itchy jealous lazy mysterious nervous obnoxious panicky pitiful repulsive scary thoughtless uptight worried broad chubby crooked curved deep flat high hollow low narrow refined round shallow skinny square steep straight wide big colossal fat gigantic great huge immense large little mammoth massive microscopic miniature petite puny scrawny short small tall teeny tiny crashing deafening echoing faint harsh hissing howling loud melodic noisy purring quiet rapping raspy rhythmic screeching shrilling squeaking thundering tinkling wailing whining whispering ancient brief early fast future late long modern old old-fashioned prehistoric quick rapid short slow swift young acidic bitter cool creamy delicious disgusting fresh greasy juicy hot moldy nutritious nutty putrid rancid ripe rotten salty savory sour spicy spoiled stale sweet tangy tart tasteless tasty yummy breezy bumpy chilly cold cool cuddly damaged damp dirty dry flaky fluffy freezing greasy hot icy loose melted prickly rough shaggy sharp slimy sticky strong tight uneven warm weak wet wooden abundant billions enough few full hundreds incalculable limited little many most millions numerous scarce some sparse substantial thousands"

{-
Adverbs:
  https://www.enchantedlearning.com/wordlist/adverbs.shtml
  https://grammar.yourdictionary.com/parts-of-speech/adverbs/list-of-100-adverbs.html
  https://7esl.com/list-of-adverbs/
  https://www.talkenglish.com/vocabulary/top-250-adverbs.aspx
  https://usefulenglish.ru/writing/list-of-adverbs
-}
adverbs = "boldly bravely brightly cheerfully deftly devotedly eagerly elegantly faithfully fortunately gleefully gracefully happily honestly innocently justly kindly merrily obediently perfectly politely powerfully safely victoriously warmly vivaciously angrily anxiously badly boastfully foolishly hopelessly irritably jealously lazily obnoxiously poorly rudely selfishly wearily always eventually finally frequently hourly never occasionally often rarely regularly seldom sometimes usually weekly yearly promptly quickly rapidly slowly speedily tediously accidentally awkwardly blindly coyly crazily defiantly deliberately doubtfully dramatically dutifully enormously evenly exactly hastily hungrily inquisitively loosely madly mortally mysteriously nervously only seriously shakily sharply silently solemnly sternly technically unexpectedly wildly"


{-
Pronouns:
  https://www.thefreedictionary.com/List-of-pronouns.htm
  https://www.really-learn-english.com/list-of-pronouns.html
  https://www.ef.com/ca/english-resources/english-grammar/pronouns/
  https://www.englishclub.com/vocabulary/pronouns-list.php
-}
pronouns = "all another any anybody anyone anything aught both each each either enough everybody everyone everything few he her hers herself him himself his I it itself many me mine most myself naught neither no nobody none nothing nought one other others ought ours ourself ourselves several she some somebody someone something such suchlike that thee theirs theirself theirselves them themself themselves there these they thine this those thou thy thyself us we what whatever whatsoever whether which whichever whichsoever who whoever whom whomever whomso whomsoever whose whosever whosesoever whoso whosoever ye yon you yours yourself yourselves"


{-
Conjunctions:
  https://www.englishclub.com/grammar/conjunctions.htm
  https://www.grammarly.com/blog/conjunctions/
  https://dictionary.cambridge.org/grammar/british-grammar/conjunctions
-}
conjunctions = "after also although and as because before but either for how if neither nor once only or since so than that though till until when where whereas whether while yet"
