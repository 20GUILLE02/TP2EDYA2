
module TestArr where

import ListSeq
import Seq

main :: IO ()
main = do
    let xs = fromList [1..5] :: [Int]
    putStrLn "Secuencia original:"
    print xs

    putStrLn "\nlengthS:"
    print $ lengthS xs

    putStrLn "\nnthS (posición 2):"
    print $ nthS xs 2

    putStrLn "\ntabulateS (*2) 5:"
    print (tabulateS (*2) 5 :: [Int])

    putStrLn "\nmapS (+1):"
    print $ mapS (+1) xs

    putStrLn "\nfilterS even:"
    print $ filterS even xs

    putStrLn "\nappendS [1,2] [3,4]:"
    print $ appendS [1,2] [3,4]

    putStrLn "\ntakeS 3:"
    print $ takeS xs 3

    putStrLn "\ndropS 2:"
    print $ dropS xs 2

    putStrLn "\nshowtS:"
    print $ showtS xs

    putStrLn "\nshowlS:"
    print $ showlS xs

    putStrLn "\njoinS [[1,2],[3,4],[5]]:"
    print $ joinS [[1,2],[3,4],[5]]

    putStrLn "\nreduceS (+) 0:"
    print $ reduceS (+) 0 xs

    putStrLn "\nscanS (+) 0:"
    print $ scanS (+) 0 xs




