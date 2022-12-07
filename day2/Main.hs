module Main where

import Data.Maybe (fromJust)
import Data.String.Utils (strip)

-- [(Oponents mv, (Winning mv, loosing mv))]
outcomeTable :: [(Char, (Char, Char))]
outcomeTable = [('A', ('Y', 'Z')), ('B', ('Z', 'X')), ('C', ('X', 'Y'))]

ptsTable :: [(Char, Integer)]
ptsTable = [('X', 1), ('Y', 2), ('Z', 3)]

-- (oponent's move, my move)
rpsOutcome :: (Char, Char) -> Integer
rpsOutcome (o, m) =
    let (win, loose) = fromJust $ lookup o outcomeTable
     in if m == win
            then 6 + fromJust (lookup m ptsTable)
            else if m == loose
                     then fromJust $ lookup m ptsTable
                     else 3 + fromJust (lookup m ptsTable)

-- (oponent's move, game end)
rps2 :: (Char, Char) -> Integer
rps2 (o, s) =
    let (win, loose) = fromJust $ lookup o outcomeTable
     in if s == 'X'
            then rpsOutcome (o, loose)
            else if s == 'Y'
                     then rpsOutcome (o, translate o)
                     else rpsOutcome (o, win)
  where
    translate 'A' = 'X'
    translate 'B' = 'Y'
    translate 'C' = 'Z'

main :: IO ()
main = do
    d <- readFile "./input.txt"
    let pts =
            sum . map (rpsOutcome . (\s -> (head s, (last . strip) s))) . lines $
            d
    putStrLn "Answer part 1:"
    print pts
    let pts2 =
            sum . map (rps2 . (\s -> (head s, (last . strip) s))) . lines $
            d
    putStrLn "Answer part 2:"
    print pts2
    return ()
