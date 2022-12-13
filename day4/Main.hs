module Main where

import Data.Set (Set, fromList, intersection, toList)
import GHC.Utils.Misc (count, split)

data Pair =
    Pair [Integer] [Integer]
    deriving (Show, Eq)

main :: IO ()
main = do
    assignments <- readFile "./input.txt" >>= pure . map parseLine . lines
    print $ take 20 assignments
    putStrLn "Answer part 1:"
    print $ count (== True) $ map isSubSet assignments
    putStrLn "Answer part 2:"
    print $ overlapPairs assignments
    return ()

-- If one member of a pair is contained withing the other
isSubSet :: Pair -> Bool
isSubSet (Pair [min1, max1] [min2, max2]) =
    (min1 >= min2 && max1 <= max2) || (min1 <= min2 && max1 >= max2)

-- If True in list, the pair overlaps with some other pair in the list
overlapPairs :: [Pair] -> Int
overlapPairs ps =
    sum [if intersects p then 1 else 0 | p <- ps]
  where
    intersects (Pair [min1, max1] [min2, max2]) =
        (min1 <= min2 && min2 <= max1) || (min2 <= min1 && min1 <= max2)

parseLine :: String -> Pair
parseLine =
    (\[l1, l2] -> Pair l1 l2) .
    map (map (read :: String -> Integer) . split '-') . split ','
