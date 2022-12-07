module Main where

import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList, intersection, singleton)

main :: IO ()
main = do
    packs <- readFile "./input.txt" >>= pure . lines
    -- Divide each pack into half, find duplicate and priority number, sum them
    let score =
            sum .
            map
                (priority .
                 head .
                 findDup .
                 (\s ->
                      let l = length s `div` 2
                       in chunksOf l s)) $
            packs
    putStrLn "Answer part 1:"
    print score
    -- Divide into groups of 3, find duplicate among group and priority number
    let badgeSum = sum . map (priority . head . findDup) $ chunksOf 3 packs
    putStrLn "Answer part 2:"
    print badgeSum
    return ()

priority :: Char -> Integer
priority c =
    fromIntegral . (1 +) . fromJust $ elemIndex c (['a' .. 'z'] ++ ['A' .. 'Z'])

findDup :: Ord a => [[a]] -> [a]
findDup [x1, x2] = toList $ intersection (fromList x1) (fromList x2)
findDup (x:xs) = toList $ intersection (fromList x) (fromList $ findDup xs)
