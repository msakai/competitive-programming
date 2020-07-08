{-# OPTIONS_GHC -Wall #-}
import Control.Monad
import Data.List
import Data.Ord
import System.IO
import Text.Printf

main :: IO ()
main = do
  ls <- liftM lines $ readFile "q3_in.txt"
  withFile "q3_out.txt" WriteMode $ \h -> do
    forM_ ls $ \l -> do
      let [n, a1] = map read $ words l
      hPrintf h "%d\n" (solve n a1)

solve :: Int -> Int -> Int
solve n a1 = sum $ evens $ solve' n a1
  where
    evens (_:x:xs) = x : evens xs
    evens _ = []

data Pos = PosLeft | PosMiddle | PosRight
  deriving (Eq, Ord, Show)

data Tree a
  = Leaf
  | Branch a (Tree a) (Tree a)

solve' :: Int -> Int -> [Int]
solve' n a1 = a1 : map fst (sortOn (\(a, dis) -> (Down dis, a)) xs)
  where
    xs :: [(Int, Int)]
    xs = collect (f PosLeft 1 (a1-1)) $ collect (f PosRight (a1+1) n) $ []

    f :: Pos -> Int -> Int -> Tree (Int, Int)
    f _pos beg end | end - beg + 1 <= 0 = Leaf
    f PosLeft   beg end = Branch (beg, end - beg) (f PosMiddle (beg+1) end) Leaf
    f PosRight  beg end = Branch (end, end - beg) Leaf (f PosMiddle beg (end-1))
    f PosMiddle beg end =
     let a = (beg + end) `div` 2
      in Branch (a, (end - beg) `div` 2) (f PosMiddle beg (a-1)) (f PosMiddle (a+1) end)

    collect :: Tree a -> [a] -> [a]
    collect Leaf = id
    collect (Branch a t1 t2) = (a :) . collect t1 . collect t2
