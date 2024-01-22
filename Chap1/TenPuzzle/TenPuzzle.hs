{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Char (isNumber)

main = do
  print $ calcPoland "612+*8-"
  print $ decodePoland "612+*8-"


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

decodePoland :: String -> Maybe String
decodePoland s = go s []
  where
    go :: String -> [String] -> Maybe String
    go [] [x] = Just $ reverse x
    go [] _   = Nothing
    go (c:s) x       | isNumber c = go s ([c]:x)
    go (c:s) (a:b:x) | isOp     c = go s ((applyOp a b c):x)
                     | otherwise  = Nothing
    applyOp a b '*' = (enclose a) ++ " * " ++ (enclose b)
    applyOp a b '/' = (enclose a) ++ " / " ++ (enclose b)
    applyOp a b '+' = a ++ " + " ++ b
    applyOp a b '-' = a ++ " - " ++ b
    
    enclose :: String -> String
    enclose [c] = [c]
    enclose s = ")" ++ s ++ "("



isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False
