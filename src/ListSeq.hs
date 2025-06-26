module ListSeq where
import Seq

instance Seq [] where
    emptyS :: [a]
    emptyS = []

    singletonS :: a -> [a]
    singletonS x = [x]    
    
    lengthS :: [a] -> Int
    lengthS [] = 0
    lengthS (_:xs) = 1 + lengthS xs

    nthS :: [a] -> Int -> a
    nthS (x:xs) 0 = x
    nthS (x:xs) n = nthS xs (n - 1)
    
    tabulateS :: (Int -> a) -> Int -> [a]
    tabulateS f n = [f i | i <- [0..n-1]]

    mapS :: (a -> b) -> [a] -> [b]
    mapS f [] = []
    mapS f (x:xs) = f x : mapS f xs

    filterS :: (a -> Bool) -> [a] -> [a]
    filterS _ [] = []
    filterS p (x:xs) | p x       = x : filterS p xs
                     | otherwise = filterS p xs

    appendS :: [a] -> [a] -> [a]
    appendS xs ys = xs ++ ys

    takeS :: [a] -> Int -> [a]
    takeS [] n = []
    takeS xs 0 = []
    takeS (x:xs) n = if n <= 0 then []
                           else x : takeS xs (n - 1)

    dropS :: [a] -> Int -> [a]
    dropS [] n = []
    dropS xs 0 = xs
    dropS (x:xs) n = dropS xs (n - 1)

    showtS :: [a] -> TreeView a [a]
    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS xs = NODE (takeS xs h) (dropS xs h)
      where h = length xs `div` 2

    showlS :: [a] -> ListView a [a]
    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS :: [[a]] -> [a]
    joinS [] = []
    joinS (x:xs) = x ++ joinS xs

    reduceS :: (a -> a -> a) -> a -> [a] -> a
    reduceS _ b [] = b
    reduceS f b (x:xs) = f x (reduceS f b xs)

    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS _ b [] = ([], b)
    scanS f b (x:xs) = let (ys, r) = scanS f (f b x) xs
                       in (b : ys, r)

    fromList :: [a] -> [a]
    fromList xs = xs

