{-# OPTIONS_GHC -Wall #-}
import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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
    fromList :: [(Int, Int, Pos)] -> IntMap (IntMap (Int, Pos, Int))
    fromList xs = IntMap.fromListWith IntMap.union $
      [ (dis, IntMap.singleton beg (end, pos, a))
      | (beg, end, pos) <- xs
      , let (a, dis) =
                case pos of
                  PosLeft -> (beg, end - beg)
                  PosRight -> (end, end - beg)
                  PosMiddle -> ((beg + end) `div` 2, (end - beg) `div` 2)
      , end - beg + 1 > 0
      ]

    go :: IntMap (IntMap (Int, Pos, Int)) -> [Int]
    go ms =
      case IntMap.maxViewWithKey ms of
        Nothing -> []
        Just ((dis, m), ms') ->
          case IntMap.minViewWithKey m of
            Nothing -> undefined
            Just ((beg, (end, pos, a)), m') ->
              let (pos1, pos2) =
                    case pos of
                      PosMiddle -> (PosMiddle, PosMiddle)
                      PosLeft   -> (PosLeft, PosMiddle)
                      PosRight  -> (PosMiddle, PosRight)
               in a : go (IntMap.unionWith IntMap.union
                            (if IntMap.null m' then ms' else IntMap.insert dis m' ms')
                            (fromList [(beg, a-1, pos1), (a+1, end, pos2)]))
