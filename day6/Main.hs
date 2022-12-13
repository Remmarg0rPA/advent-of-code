module Main where

main :: IO ()
main = do
  dataStream <- readFile "input.txt"
  putStrLn "Answer part 1:"
  print $ findMarker dataStream 4
  putStrLn "Answer part 2:"
  print $ findMarker dataStream 14
  return ()

unique :: Eq a => [a] -> Bool
unique [] = True
unique (a:as) = (a `notElem` as) && unique as

findMarker :: String -> Int -> Int
findMarker s n = if unique (take n s) then n else 1 + findMarker (tail s) n
