module ArrSeq where
import qualified Arr as A
import Arr ((!))  -- para usar el operador `!` directamente
import Seq (Seq(..), TreeView(..), ListView(..))
import Language.Haskell.TH (safe)
import Par

instance Seq A.Arr where
    emptyS :: A.Arr a
    emptyS = A.fromList []

    singletonS :: a -> A.Arr a
    singletonS x = A.fromList [x]

    lengthS :: A.Arr a -> Int
    lengthS = A.length

    nthS :: A.Arr a -> Int -> a
    nthS = (!)

    takeS :: A.Arr a -> Int -> A.Arr a
    takeS arr n = A.subArray 0 n arr

    dropS :: A.Arr a -> Int -> A.Arr a
    dropS arr n = A.subArray n (A.length arr - n) arr

    appendS :: A.Arr a -> A.Arr a -> A.Arr a
    appendS s1 s2 = A.tabulate f totalLen
      where len1 = A.length s1
            len2 = A.length s2
            totalLen = len1 + len2
            f i = if i < len1 then s1 ! i -- devuelvo el elemento i de s1 
                              else s2 ! (i - len1) -- devuelvo el elemento i - len1 de s2

    fromList :: [a] -> A.Arr a
    fromList = A.fromList

    joinS :: A.Arr (A.Arr a) -> A.Arr a
    joinS = A.flatten

    reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
    reduceS f e arr = case lengthS arr of
        0 -> e
        1 -> f e (nthS arr 0)
        otherwise -> let ct = contract f arr in reduceS f e ct

    tabulateS :: (Int -> a) -> Int -> A.Arr a
    tabulateS = A.tabulate

    mapS :: (a -> b) -> A.Arr a -> A.Arr b
    mapS f s = A.tabulate (\i -> f (s ! i)) (A.length s)

    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
    filterS f arr = A.fromList [arr ! i | i <- [0 .. A.length arr - 1], f (arr ! i)]

    scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
    scanS f e arr = case lengthS arr of
        0 -> (emptyS, e)
        1 -> (singletonS e, f e (nthS arr 0))
        otherwise -> let
                        (s', r) = scanS f e ct
                        ct = contract f arr
                    in (expand f s' arr, r)

    showtS s
        | A.length s == 0 = EMPTY
        | A.length s == 1 = ELT (s ! 0)
        | otherwise       = let
            mid = A.length s `div` 2
            left = A.subArray 0 mid s
            right = A.subArray mid (A.length s - mid) s
          in NODE left right

    showlS s
        | A.length s == 0 = NIL
        | otherwise       = CONS (s ! 0) (A.subArray 1 (A.length s - 1) s)


contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f xs = case lengthS xs of
    0 -> emptyS
    1 -> xs
    otherwise -> let
                    x = nthS xs 0
                    y = nthS xs 1
                    (l, ys) = f x y ||| rest
                    rest = contract f (dropS xs 2)
                in appendS (singletonS l) ys

expand :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expand f xs ys = case lengthS ys of
    0 -> xs
    1 -> xs
    otherwise -> let
                    x = nthS xs 0
                    y1 = nthS ys 0
                    y2 = nthS ys 1
                    xs' = dropS xs 1
                    ys' = dropS ys 2
                    ri = (f x y1)
                    rs = expand f xs' ys'
                in appendS (appendS (singletonS x) (singletonS ri)) rs