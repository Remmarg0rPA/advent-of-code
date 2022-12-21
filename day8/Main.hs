module Main where

import Data.List (transpose)

main :: IO ()
main = do
    forest <-
        readFile "input.txt" >>=
        pure .
        map
            (map (read . (: []) :: (Integral a, Read a) =>
                                       Char -> a)) .
        lines
    let (width, height) = (length . head $ forest, length forest)
    let seenTrees =
            [ pos
            | x <- [0 .. width - 1]
            , y <- [0 .. height - 1]
            , let pos = (x, y)
            , canBeSeen pos forest
            ]
    let scenicScores =
            [ scenicScore pos forest
            | x <- [0 .. width - 1]
            , y <- [0 .. height - 1]
            , let pos = (x, y)
            ]
    putStrLn "Answer part 1:"
    print $ length seenTrees
    putStrLn "Answer part 2:"
    print $ maximum scenicScores
    return ()

canBeSeen :: Integral a => (a, a) -> [[a]] -> Bool
canBeSeen (x', y') forest =
    let (x, y) = (fromIntegral x', fromIntegral y')
     in if x == 0 || y == 0
            then True
            else let treeHeight = forest !! y !! x
                  in or $
                     map
                         (treeHeight >)
                         [ (maximum $ -1 : (take x $ forest !! y))
                         , (maximum $ -1 : (drop (x + 1) $ forest !! y))
                         , (maximum $ -1 : (take y $ (transpose forest) !! x))
                         , (maximum $
                            -1 : (drop (y + 1) $ (transpose forest) !! x))
                         ]

scenicScore :: Integral a => (a, a) -> [[a]] -> a
scenicScore (x', y') forest =
    let (x, y) = (fromIntegral x', fromIntegral y')
        treeHeight = forest !! y !! x
     in if x == 0 || y == 0
            then 0
            else product $
                 map
                     (sightDist treeHeight)
                     [ reverse $ take x $ forest !! y
                     , drop (x + 1) $ forest !! y
                     , reverse $ take y $ (transpose forest) !! x
                     , drop (y + 1) $ (transpose forest) !! x
                     ]
  where
    sightDist _ [] = 0
    sightDist treeHeight (t:ts) =
        if t < treeHeight
            then 1 + (sightDist treeHeight ts)
            else if t == treeHeight
                     then 1
                     else 0
