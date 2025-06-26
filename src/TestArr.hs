-- Main.hs
module TestArr where

import Seq (Seq(..))
import qualified Arr as A
import qualified ArrSeq () -- Importar para que se cargue la instancia


main :: IO ()
main = do
  let arr = fromList [1..10] :: A.Arr Int

  putStrLn "Array inicial:"
  printArr arr

  putStrLn "\nemptyS:"
  printArr (emptyS :: A.Arr Int)

  putStrLn "\nsingletonS 42:"
  printArr (singletonS 42)

  putStrLn "\nnthS arr 3:"
  print (nthS arr 3)

  putStrLn "\ntakeS arr 5:"
  printArr (takeS arr 5)

  putStrLn "\ndropS arr 5:"
  printArr (dropS arr 5)

  putStrLn "\nappendS (takeS arr 3) (dropS arr 3):"
  printArr (appendS (takeS arr 3) ( dropS arr 3))

  putStrLn "\nmapS (*2) arr:"
  printArr (mapS (*2) arr)

  putStrLn "\nfilterS even arr:"
  printArr (filterS even arr)

  putStrLn "\nreduceS (+) 0 arr:"
  print (reduceS (+) 0 arr)

  putStrLn "\nscanS (+) 0 arr:"
  let (scanRes, lastVal) = scanS (+) 0 arr
  printArr scanRes
  putStrLn ("Last value: " ++ show lastVal)

  putStrLn "\nshowtS arr:"
  print (showtS arr)

  putStrLn "\nshowlS arr:"
  print (showlS arr)

-- Función auxiliar para imprimir un A.Arr asumiendo que tiene Show
printArr :: Show a => A.Arr a -> IO ()
printArr arr = print [arr A.! i | i <- [0 .. A.length arr - 1]]
