{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lib

linesToLists :: T.Text -> [[Int]]
linesToLists lines = map (map textToInt) (map T.words (T.lines lines))

safe :: [Int] -> Bool
safe levels = 
    (all (\x -> abs x >= 1 && abs x <= 3) levels') && (all (< 0) levels' || all (> 0) levels')
    where 
        levels' = zipWith (-) levels (tail levels)

part1 :: [[Int]] -> Int
part1 lines = length $ filter safe lines

part2 = undefined

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day2.txt"
    let lists = linesToLists contents
    print $ part1 lists
    -- print $ part2 lists


test = T.pack "7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9\n"
