module Main where

import Control.Lens.Lens ((??))
import Data.List (transpose)
import GHC.Utils.Misc (filterOut, split)

type Move = [Int]
type Stacks = [[Char]]

main :: IO ()
main = do
    file_lines <- readFile "input.txt" >>= pure . lines
    let stacks = parseStacks $ takeWhile (/= "") file_lines
        moves = map parseMove . drop 1 $ dropWhile (/= "") file_lines
    putStrLn "Answer part 1:"
    print $ map head $ foldl makeMove stacks moves
    putStrLn "Answer part 2:"
    print $ map head $ foldl makeMove2 stacks moves
    return ()

parseMove :: String -> Move
parseMove = map read . (??) [(!! 1), (!! 3), (!! 5)] . split ' '
                        -- ["move", _, "from", _, "to", _]

parseStacks :: [String] -> Stacks
parseStacks s =
    map (foldr
             (\c acc ->
                  if c /= ' '
                      then c : acc
                      else acc)
             []) . -- Filter out the spaces
    transpose $ -- Trannspose to make it a list of the stacks
    [[line !! i | i <- stackIndicies] | line <- (init s)] -- Extract the characters in each row
  where
    ncols =
        (read :: String -> Int) . last . filterOut null . split ' ' . last $ s
    stackIndicies = [1 + 4 * i | i <- [0 .. ncols - 1]] -- the index of what is in each column in the stack

makeMove :: Stacks -> Move -> Stacks
makeMove stacks [n, from, to] =
    [ if idx == (to - 1)
        then toStack
        else if idx == (from - 1)
                 then fromStack
                 else stacks !! idx
    | idx <- [0 .. length stacks - 1]
    ]
  where
    moveCrates = take n $ stacks !! (from-1)
    toStack = (reverse moveCrates) ++ (stacks !! (to-1))
    fromStack = drop n $ stacks !! (from-1)

makeMove2 :: Stacks -> Move -> Stacks
makeMove2 stacks [n, from, to] =
    [ if idx == (to - 1)
        then toStack
        else if idx == (from - 1)
                 then fromStack
                 else stacks !! idx
    | idx <- [0 .. length stacks - 1]
    ]
  where
    moveCrates = take n $ stacks !! (from-1)
    toStack = moveCrates ++ (stacks !! (to-1))
    fromStack = drop n $ stacks !! (from-1)
