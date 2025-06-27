module ListSeq where
import Seq
import Par

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
    reduceS f e [] = e
    reduceS f e [x] = f e x
    reduceS f e xs = let
                      ct = contract f xs
                    in reduceS f e ct

    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS f e []  = (emptyS, e)
    scanS f e [x] = (singletonS e, f e x)
    scanS f e xs  = let 
                        (s',r) = scanS f e ct
                        ct = contract f xs 
                    in (expand f s' xs, r)

    fromList :: [a] -> [a]
    fromList xs = xs


contract :: (a -> a -> a) -> [a] -> [a]
contract f [] = []
contract f [x] = [x]
contract f (x:y:xs) = let 
                          (l, ys) = f x y ||| rest
                          rest = contract f xs
                        in l : ys

expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f xs [] = xs
expand f xs [y] = xs
expand f (x:xs) (y1:y2:ys) = let 
                                ri = (f x y1)
                                rs = expand f xs ys
                              in x:ri:rs