{-# OPTIONS_GHC -Wall #-}
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

solve' :: Int -> Int -> [Int]
solve' n a1 = a1 : go (fromList [(1, a1-1, PosLeft), (a1+1, n, PosRight)])
  where
    fromList :: [(Int, Int, Pos)] -> Map (Down Int, Int) (Int, Pos, Int)
    fromList xs = Map.fromList $
      [ ((Down dis, beg), (end, pos, a))
      | (beg, end, pos) <- xs
      , let (a, dis) =
                case pos of
                  PosLeft -> (beg, end - beg)
                  PosRight -> (end, end - beg)
                  PosMiddle -> ((beg + end) `div` 2, (end - beg) `div` 2)
      , end - beg + 1 > 0
      ]

    go :: Map (Down Int, Int) (Int, Pos, Int) -> [Int]
    go ms =
      case Map.minViewWithKey ms of
        Nothing -> []
        Just (((_dis, beg), (end, pos, a)), m') ->
          let (pos1, pos2) =
                case pos of
                  PosMiddle -> (PosMiddle, PosMiddle)
                  PosLeft   -> (PosLeft, PosMiddle)
                  PosRight  -> (PosMiddle, PosRight)
           in a : go (Map.union m' (fromList [(beg, a-1, pos1), (a+1, end, pos2)]))
