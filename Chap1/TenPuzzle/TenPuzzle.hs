{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Char (isNumber)
import Control.Monad (guard, MonadPlus, mzero)
import System.Environment (getArgs)

main = do
  -- print $ calcPoland "612+*8-"
  -- print $ calcPoland "543*-"
  -- print $ (decodePoland "612+*8-" :: Maybe String)
  args <- getArgs
  let num = head $ args ++ ["1234"]
  mapM_ putStrLn $ solve num
  -- putStrLn $ head $ solve "6128"



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

    go (c:s) z
      | isNumber c = go s (read [c]:z)
      | otherwise  = Nothing
    apply :: Double -> Double -> [Double] -> (Double -> Double -> Double) -> [Double]
    apply x y z op = (op y x):z

decodePoland :: MonadPlus m => String -> m String
decodePoland s = go s []
  where
    go :: MonadPlus m => String -> [String] -> m String
    go [] [x] = return $ reverse x
    go [] _   = mzero
    go (c:s) x | isNumber c = go s ([c]:x)
    go (c:s) (a:b:x)
      | isOp     c = go s ((applyOp a b c):x)
      | otherwise  = mzero
    applyOp a b op = (enclose a) ++ [' ', op, ' '] ++ (enclose b)

    enclose :: String -> String
    enclose [c] = [c]
    enclose s = ")" ++ s ++ "("



isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False

genOps :: [(Char, Char, Char)]
genOps = [(a, b, c) | a <- ops, b <- ops, c <- ops]
  where ops = "+-*/"

solve :: String -> [String]
solve [a, b, c, d] = do
  (x, y, z) <- genOps
  s <- [
      [a, b, c, d, x, y, z],
      [a, b, c, x, d, y, z],
      [a, b, c, x, y, d, z],
      [a, b, x, c, y, d, z],
      [a, b, x, c, d, y, z]
    ]
  guard $ (Just 10) == calcPoland s 
  ans <- decodePoland s
  return $ ans ++ " : " ++ s
solve _ = []

