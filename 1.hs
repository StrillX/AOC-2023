module AOC where
import Cp (split)
import Data.Char
import Data.Text (pack, unpack,replace)



getDigits :: String -> [[Char]]
getDigits =  map (filter isDigit) . lines 

concatPair :: (Char, Char) -> Int
concatPair (a, b) = read (a : [b])


one :: IO ()
one = do
  input <- readFile "./1.txt"
  print $ sum  $ map (concatPair . split head last) (getDigits input)


-- prevenir casos tipo eightwo de ficarem eigh2 quando chega ao 8
subs = [ ("one", "o1e")
       , ("two", "t2o")
       , ("three", "t3e")
       , ("four", "f4r")
       , ("five", "f5e")
       , ("six", "s6x")
       , ("seven", "s7n")
       , ("eight", "e8t")
       , ("nine", "n9e")
       ]


scuffedReplace = foldl (\ text sub -> replace (pack $ fst sub) (pack $ snd sub) text)


two :: IO ()
two = do
  input <- readFile "./1.txt"
  let text = pack input
  let replaced = unpack $ scuffedReplace text subs
  print $ sum $ map (concatPair . split head last) (getDigits replaced)