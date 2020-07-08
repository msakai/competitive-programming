import Control.Monad
import Data.Monoid
import Text.Printf
import System.IO

main :: IO ()
main = do
  ls <- liftM lines $ readFile "q2_in.txt"
  withFile "q2_out.txt" WriteMode $ \h -> do
    forM_ ls $ \l -> do
      let [k, p, q] = map read $ words l
          (a,b,c) = solve k p q
      hPrintf h "a:%d,b:%d,c:%d\n" a b c

solve :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
solve k p q = 
  case (count q, count (p - 1)) of
    ((a1,b1,c1), (a2,b2,c2)) ->
      ( getSum a1 - getSum a2
      , getSum b1 - getSum b2
      , getSum c1 - getSum c2
      )
  where
    -- table !! i は S_{i+1} に含まれる a, b, c の個数
    table :: [(Sum Integer, Sum Integer, Sum Integer)]
    table
      = (Sum 1, Sum 0, Sum 0)
      : (Sum 0, Sum 1, Sum 0)
      : (Sum 0, Sum 0, Sum 1)
      : zipWith3 (\x1 x2 x3 -> x1 <> x2 <> x3) table table' table''
    table' = tail table
    table'' = tail table'

    -- S_k の n 文字目までに含まれる a, b, c の個数
    count :: Integer -> (Sum Integer, Sum Integer, Sum Integer)
    count n = f n (reverse $ take (fromInteger k) $ table) mempty
      where
        f 0 _ ret = ret
        f n (x : xs) ret
          | n == sum3 x = ret <> x
          | n > sum3 x2 + sum3 x3 = f (n - sum3 x2 - sum3 x3) (x1 : x2 : x3 : xs') (ret <> x2 <> x3)
          | n >           sum3 x3 = f (n - sum3 x3) (x2 : x3 : xs') (ret <> x3)
          | otherwise             = f n (x3 : xs') ret
          where
            x1 : x2 : x3 : xs' = xs
            sum3 (a,b,c) = getSum $ a <> b <> c
