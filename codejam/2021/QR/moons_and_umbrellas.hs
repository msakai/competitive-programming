{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Ord
import Text.Printf

main :: IO ()
main = do
  t <- readLn
  forM_ [(1::Int)..t] $ \k -> do
    [x_, y_, s] <- liftM words getLine
    let x = read x_
        y = read y_
    let sol = cost x y $ solve x y s
    printf "Case #%d: %d\n" k sol

costOne :: Int -> Int -> Char -> Char -> Int
costOne x _y 'C' 'J' = x
costOne _x y 'J' 'C' = y
costOne _ _ _ _ = 0

cost :: Int -> Int -> String -> Int
cost x y = go 0 Nothing
  where
    go !val _ [] = val
    go !val Nothing (c:cs) = go val (Just c) cs
    go !val (Just prev) (c:cs) = go (val + costOne x y prev c) (Just c) cs

solve :: Int -> Int -> String -> String
solve x y = go Nothing
  where
    go _ [] = []
    go c1 ccs@('?':_) =
      case span ('?'==) ccs of
        (qs, []) -> solve' c1 Nothing (length qs)
        (qs, c2:cs') -> solve' c1 (Just c2) (length qs) ++ c2 : go (Just c2) cs'
    go _ (c:cs) = c : go (Just c) cs

    table :: Map (Char, Char, Int) (String, Int)
    table = Map.fromListWith (\v1@(_,cost1) v2@(_,cost2) -> if cost1 < cost2 then v1 else v2) $
      [((c,c,1), ([c], 0)) | c <- "CJ"] ++
      [ ((beg1, end2, n), (s3, cost3))
      | n <- [2..1002]
      , beg1 <- "CJ"
      , end2 <- "CJ"
      , let (s3, cost3) = minimumBy (comparing snd) $ do
              end1 <- "CJ"
              beg2 <- "CJ"
              (s1, cost1) <- maybeToList $ Map.lookup (beg1, end1, n `div` 2) table
              (s2, cost2) <- maybeToList $ Map.lookup (beg2, end2, n - (n `div` 2)) table
              return (s1 ++ s2, cost1 + cost2 + costOne x y end1 beg2)
      ]

    solve' :: Maybe Char -> Maybe Char -> Int -> String
    solve' (Just c1) (Just c2) n = init $ tail $ fst $ table Map.! (c1, c2, n+2)
    solve' (Just c1) Nothing n = tail $ fst $ minimumBy (comparing snd) $ catMaybes [Map.lookup (c1, c2, n+1) table | c2 <- "CJ"]
    solve' Nothing (Just c2) n = init $ fst $ minimumBy (comparing snd) $ catMaybes [Map.lookup (c1, c2, n+1) table | c1 <- "CJ"]
    solve' Nothing Nothing n = fst $ minimumBy (comparing snd) $ catMaybes [Map.lookup (c1, c2, n) table | c1 <- "CJ", c2 <- "CJ"]
