{-# OPTIONS_GHC -Wall #-}
import Control.Monad
import Data.Ord
import qualified Data.Set as Set
import Data.Set (Set)
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

solve' :: Int -> Int -> [Int]
solve' n a1 = a1 : (map snd $ Set.toList $ f PosLeft 1 (a1-1) `Set.union` f PosRight (a1+1) n)
  where
    f :: Pos -> Int -> Int -> Set (Down Int, Int)
    f _pos beg end | end - beg + 1 <= 0 = Set.empty
    f PosLeft   beg end = Set.insert (Down (end - beg), beg) $ f PosMiddle (beg+1) end
    f PosRight  beg end = Set.insert (Down (end - beg), end) $ f PosMiddle beg (end-1)
    f PosMiddle beg end =
     let a = (beg + end) `div` 2
      in Set.insert (Down ((end - beg) `div` 2), a) $ f PosMiddle beg (a-1) `Set.union` f PosMiddle (a+1) end
