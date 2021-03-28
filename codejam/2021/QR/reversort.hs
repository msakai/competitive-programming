{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Writer.Strict
import Data.List
import Text.Printf

main :: IO ()
main = do
  t <- readLn
  forM_ [(1::Int)..t] $ \k -> do
    _n <- getLine
    (xs :: [Int]) <- liftM (map read . words) getLine
    let sol = reversort xs
    printf "Case #%d: %d\n" k sol

reversort :: (Ord a, Show a) => [a] -> Int
reversort xs = getSum $ execWriter $ go xs (sort xs)
  where
    go [] _ = return ()
    go [_] _ = return ()
    go xxs (y:ys) =
      case break (y==) xxs of
        (xs', _y:xs'') -> do
          tell (Sum (length xs' + 1))
          go (reverse xs' ++ xs'') ys
