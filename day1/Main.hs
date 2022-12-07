module Main where
import Data.List (sort)

main :: IO()
main = do
    caloriesList <- fmap lines $ readFile "./input.txt"
    let calories =
            foldl
                (\(lst, last) c ->
                     if c == ""
                         then (last : lst, 0)
                         else (lst, last + (read c :: Integer)))
                ([], 0)
                caloriesList
    let cals = snd calories : fst calories
    putStrLn "Answer part 1:"
    print $ maximum cals
    putStrLn "Answer part 2:"
    print . sum . take 3 . reverse . sort $ cals
    return ()
