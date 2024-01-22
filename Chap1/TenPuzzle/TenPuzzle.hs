{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Char (isNumber)

main = do
  print $ calcPoland "612+*8-"
  

calcPoland :: String -> Maybe Double
calcPoland s = go s []
  where
    go :: String -> [Double] -> Maybe Double
    go "" [x] = Just x
    go "" _   = Nothing

    go ('+':s) (x:y:z) = go s $ apply x y z (+)
    go ('-':s) (x:y:z) = go s $ apply x y z (-)
    go ('*':s) (x:y:z) = go s $ apply x y z (*)
    go ('/':s) (x:y:z) = go s $ apply x y z (/)

    go (c:s) z | isNumber c = go s (read [c]:z)
               | otherwise  = Nothing
    apply :: Double -> Double -> [Double] -> (Double -> Double -> Double) -> [Double]
    apply x y z op = (op y x):z
