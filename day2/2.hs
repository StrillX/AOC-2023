module AOC where

import Cp (split)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

buildMSet :: String -> (Int, String)
buildMSet s = (count, color)
  where
    count = read $ head $ words s :: Int
    color = unwords $ tail $ words s

parseLine :: String -> [String]
parseLine line = map trim $ splitOn "," [if l `elem` ";:" then ',' else l | l <- line]

processLine :: String -> [(Int, String)]
processLine = map buildMSet . tail . parseLine

validMSet :: (Int, String) -> Bool
validMSet (count, color)
  | color == "blue" && count <= 14 = True
  | color == "green" && count <= 13 = True
  | color == "red" && count <= 12 = True
  | otherwise = False

possible = map fst . filter snd . zip [1 ..] . map (all validMSet . processLine) . lines

one :: IO ()
one = do
  input <- readFile "2.txt"
  print $ sum $ possible input

-- part 2
maxColor :: String -> [(Int, String)] -> (Int, String)
maxColor color mset = (maximum (map fst (filter (\(count, col) -> col == color) mset)), color)

maxMSets :: [(Int, String)] -> [(Int, String)]
maxMSets mset = [maxBlue, maxGreen, maxRed]
  where
    maxBlue = maxColor "blue" mset
    maxGreen = maxColor "green" mset
    maxRed = maxColor "red" mset

products = map (product . map fst . maxMSets . processLine) . lines

two :: IO ()
two = do
  input <- readFile "2.txt"
  print $ sum $ products input