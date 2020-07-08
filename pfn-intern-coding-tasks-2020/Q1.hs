import Control.Monad
import System.IO
import Text.Printf

main :: IO ()
main = do
  ls <- liftM lines $ readFile "q1_in.txt"
  withFile "q1_out.txt" WriteMode $ \h -> do
    forM_ ls $ \l -> do
      let [x, y, d] = map read $ words l
      hPrintf h "%d\n" (solve x y d)

det :: Num a => [a] -> a
det
  [ x11, x12, x13
  , x21, x22, x23
  , x31, x32, x33
  ] = x11*x22*x33 + x12*x23*x31 + x13*x21*x32 - x13*x22*x31 - x12*x21*x33 - x11*x23*x32

solve :: Integer -> Integer -> Integer -> Int
solve x y d = length [() | xs <- replicateM 9 [x,y], d == det xs]
