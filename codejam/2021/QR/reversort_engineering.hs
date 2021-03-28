{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Maybe
import Text.Printf

main :: IO ()
main = do
  t <- readLn
  forM_ [(1::Int)..t] $ \k -> do
    [n, c] <- liftM (map read . words) getLine
    case solve n c of
      Nothing -> printf "Case #%d: IMPOSSIBLE\n" k
      Just xs -> printf "Case #%d: %s\n" k (intercalate " " (map show xs))

solve :: Int -> Int -> Maybe [Int]
solve n c = listToMaybe $ liftM IntMap.elems $ go 1 [1..n] n c
  where
    go i ps n c
     | n == 1 = assert (tail ps == []) $ guard (c == 0) >> return (IntMap.singleton (head ps) i)
     | c < n-1 = mzero
     | c > n*(1+n) `div` 2 - 1 = mzero
     | otherwise = do
         (len,p) <- zip [1..] ps
         let ps' = tail (reverse (take len ps)) ++ drop len ps
         assert (last (take len ps) == p) $ return ()
         m <- go (i+1) ps' (n - 1) (c - len)
         return $ IntMap.insert p i m
