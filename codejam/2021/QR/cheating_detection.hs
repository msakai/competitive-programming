{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Ord
import Text.Printf

main :: IO ()
main = do
  (t::Int) <- readLn
  (_p::Int) <- readLn
  forM_ [(1::Int)..t] $ \k -> do
    xss <- replicateM 100 $ liftM (map ('1'==)) getLine
    let a = array ((1,1),(100,10000)) [((i,j), x) | (i,xs) <- zip [1..] xss, (j,x) <- zip [1..] xs]
    printf "Case #%d: %d\n" k (solve a)

solve :: UArray (Int,Int) Bool -> Int
solve a = fst $ minimumBy (comparing snd) [(i, auc i) | i <- [1..100]]
  where
    ps :: UArray Int Int
    ps = array (1,10000) [(j, length [() | i <- [1..100], a ! (i,j)]) | j <- [1..10000]]

    ps2 :: [Int]
    ps2 = sortBy (comparing (ps !)) [1..10000]

    auc :: Int -> Double
    auc i = go 0 0 0 ps2
      where
        go :: Int -> Int -> Int -> [Int] -> Double
        go !r !y !x [] = fromIntegral r / fromIntegral (y * x)
        go !r !y !x (j:js) =
          if a ! (i,j)
          then go (r + y) y (x + 1) js
          else go r (y + 1) x js
