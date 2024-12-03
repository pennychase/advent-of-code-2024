{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day1 where

import Data.List (sort)
import qualified Data.MultiSet as MS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lib

linesToLists :: [T.Text] -> ([Int], [Int])
linesToLists lines = unzip $ map ((\[x, y] -> (textToInt x, textToInt y)) . T.words) lines

part1 :: ([Int], [Int]) -> Int
part1 (as, bs) =
    sum $ zipWith (\x y -> abs (x - y)) (sort as) (sort bs)

part2 :: ([Int], [Int]) -> Int
part2 (as, bs) = 
    foldr (\x xs -> x * (MS.occur x ms) + xs) 0 as
    where
        ms = MS.fromList bs

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day1.txt"
    let lines = T.lines contents
    let lists = linesToLists lines
    print $ part1 lists
    print $ part2 lists

test = T.pack "3   4\n\
\4   3\n\
\2   5\n\
\1   3\n\
\3   9\n\
\3   3\n"
 