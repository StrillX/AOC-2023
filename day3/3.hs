module AOC where

import qualified Data.Map.Strict as Map
import Data.Char (isDigit, isSymbol)
import Data.Maybe (isJust, fromJust, mapMaybe, catMaybes)
import Text.Printf

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

getNumbers :: String -> [((Int, Int), String)]
getNumbers line = foldl aux [] (zip [0..] line)
    where aux acc (i, c) = if isDigit c
                        then case acc of
                            [] -> [((i, i), [c])]
                            all@(((s, e), num) : xs) -> if e == i - 1
                                then ((s, i), num ++ [c]) : xs
                                else ((i, i), [c]) : all
                        else acc

parseNumber :: [String] -> [((Int, Int, Int), Int)]
parseNumber input = zip [0..] (map getNumbers input) >>= aux
    where aux (row, numbers) = map (\((s, e), num) -> ((e-s+1, s, row), read num)) numbers


getSymbol :: String -> [(Int,Char)]
getSymbol = filter (\(i, c) -> AOC.isSymbol c) . zip [0..]

symbolMap :: (Ord a, Num a, Enum a) => [String] -> Map.Map (Int, a) Char
symbolMap inp = Map.fromList $ zip [0..] (map getSymbol inp) >>= aux
    where aux (row, symbols) = map (\(i, c) -> ((i, row), c)) symbols

adjacentSymbols :: Map.Map (Int, Int) Char -> Int -> Int -> Int -> [((Int, Int), Char)]
adjacentSymbols sm length x y = mapMaybe (\p -> Map.lookup p sm >>= (\c -> Just (p, c))) adjacentInds
    where adjacentInds = [(x+dx, y+dy) | dx <- [-1 .. length], dy <- [-1 .. 1], (dy == 0 && (dx == -1 || dx == length)) || dy /= 0]


validNumbers :: Map.Map (Int, Int) Char -> [((Int, Int, Int), Int)] -> [((Int, Int, Int), Int)]
validNumbers sm = filter validNumber
    where validNumber ((length, x, y), _) = not . null $ adjacentSymbols sm length x y


one :: IO()
one = do
    input <- readFile "3.txt"
    print $ sum $ map snd $ validNumbers (symbolMap $ lines input) (parseNumber $ lines input)

adjacentChars input ((length, x, y), n)  = [(p, [n]) | p <- adjacentSymbols (symbolMap $ lines input) length x y]


two :: IO()
two = do
    input <- readFile "3.txt"
    let nums = validNumbers  (symbolMap (lines input)) $ parseNumber $ lines input
    let numbers = Map.fromListWith (++) $ nums >>= adjacentChars input
   
    print $ sum [product n | n <- Map.elems numbers, length n == 2]
    