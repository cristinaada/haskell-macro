module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp s l = [y| (x,y)<-l , x==s]

split :: [Char] -> String -> (String, [String])
split seps [] = ("" , [""])
split seps (x:xs)
   |elem x seps = (x:s , ("" : m))
   |otherwise= (s , (x:(head m)):(tail m))
   where
      (s , m) = (split seps xs)

combine :: String -> [String] -> [String]
combine [] words = ((head words):[])
combine seps words =((head words):([(head seps)]:combine (tail seps) (tail words)))


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs lines = ((key , def):getKeywordDefs lines')
   where
      line = head lines
      lines' = tail lines
      key = head (snd l)
      def = concat (combine (tail (fst l)) (tail (snd l)))
      l = split " " line

expand :: FileContents -> FileContents -> FileContents
expand [] info = []
expand text [] = text
expand text info
   =concat (combine seps (replace words key))
       where
          (seps , words) = split separators text
          lines = snd (split "\n" info)
          key = getKeywordDefs lines


replace :: [String] -> KeywordDefs -> [String]
replace [] key = []
replace words [] = words
replace (x:xs) key
   |x == ""       = "" : (replace xs key)
   |head x == '$' = (replacement x key):(replace xs key)
   |otherwise     = x:(replace xs key)

replacement :: String -> KeywordDefs -> String
replacement word keys = concat [y| (x,y)<-keys , x==word]



exExpand :: FileContents -> FileContents -> FileContents
exExpand text []   = text
exExpand text infos
   = concat (exExpand' text info)
   where
      exExpand' text []   = [[]]
      exExpand' [] info   = [[]]
      exExpand' text info = (expand text (head info))  : ("-----" : "\n" : exExpand' text (tail info))
      info = snd (split "#" infos)




main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (exExpand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
