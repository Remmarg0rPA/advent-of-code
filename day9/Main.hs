module Main where

import Prelude hiding (Left, Right)
import Data.List.Utils (uniq)
import GHC.Utils.Misc (split)

data Direction
    = U
    | D
    | R
    | L
    deriving (Show, Eq, Read)

main :: IO ()
main = do
    moves <- readFile "input.txt" >>= pure . concatMap parse . lines
    let headPositions =
            reverse $
            foldl (\(a:cc) mv -> moveHead a mv : a : cc) [(0, 0)] moves
    let tailPositions =
            drop 1 . reverse $
            foldl
                (\(a:cc) pos -> moveTail pos a : a : cc)
                [(0, 0)]
                headPositions
    let t9 =
            applyN
                9
                (drop 1 .
                 reverse .
                 foldl (\(a:cc) pos -> moveTail pos a : a : cc) [(0, 0)])
                headPositions
    putStrLn "Answer part 1"
    print . length . uniq $ tailPositions
    putStrLn "Answer part 2"
    print . length . uniq $ t9
    return ()

parse :: String -> [Direction]
parse mv = replicate n dir
  where
    dir = (read :: String -> Direction) . head $ split ' ' mv
    n = (read :: (Integral a, Read a) => String -> a) . last $ split ' ' mv


moveHead :: (Num a, Eq a, Show a) => (a, a) -> Direction -> (a, a)
moveHead (x, y) U = (x, y + 1)
moveHead (x, y) D = (x, y - 1)
moveHead (x, y) R = (x + 1, y)
moveHead (x, y) L = (x - 1, y)

moveTail :: (Integral a, Eq a, Show a) => (a, a) -> (a, a) -> (a, a)
moveTail headP tailP =
    let x' =
            if dx == 0 || (abs dx == 1 && dy == 0)
                then fst tailP
                else fst tailP +
                     (if abs dx == 1
                          then dx
                          else dx `div` 2)
        y' =
            if dy == 0 || (abs dy == 1 && dx == 0)
                then snd tailP
                else snd tailP +
                     (if abs dy == 1
                          then dy
                          else dy `div` 2)
     in if (x', y') == headP
            then tailP
            else (x', y')
  where
    diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
    (dx, dy) = diff headP tailP

applyN :: Integral n => n -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)
