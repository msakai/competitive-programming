{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Control.Monad.State.Strict
import Data.List (intercalate)
import System.Exit
import System.IO

main :: IO ()
main = do
  [t, n, q] <- liftM (map read . words) getLine
  solve t n q

solve :: Int -> Int -> Int -> IO ()
solve t n q = flip evalStateT q $ do
  replicateM_ t (solve1 n)

type M = StateT Int IO

solve1 :: Int -> M ()
solve1 n = do
  let go xs [] = return xs
      go xs (y:ys) = insert y xs >>= \xs' -> go xs' ys

      insert x xs = do
        let (ys, zs) = splitAt (length xs `div` 2) xs
            y = last ys
            z = head zs
        ret <- median x y z
        if ret == x then do -- y x z
          return $ ys ++ [x] ++ zs
        else if ret == y then do -- x y z
          case init ys of
            [] -> return $ [x, y] ++ zs
            [y'] -> do
              ret2 <- median y' x y
              if ret2 == x then do
                return $ [y', x, y] ++ zs
              else if ret2 == y' then do
                return $ [x, y', y] ++ zs
              else undefined
            ys' -> do
              ys'' <- insert x ys'
              return $ ys'' ++ [y] ++ zs
        else if ret == z then do -- y z x
          case tail zs of
            [] -> return $ ys ++ [z, x]
            [z'] -> do
              ret2 <- median z x z'
              if ret2 == x then do
                return $ ys ++ [z, x, z']
              else if ret2 == z' then do
                return $ ys ++ [z, z', x]
              else undefined
            zs' -> do
              zs'' <- insert x zs'
              return $ ys ++ [z] ++ zs''
        else
          undefined

  m <- median 1 2 3
  xs <-
    case m of
      1 -> go [2, 1, 3] [4..n]
      2 -> go [1, 2, 3] [4..n]
      3 -> go [1, 3, 2] [4..n]
      _ -> undefined
  _ <- sendSolution xs
  return ()

median :: Int -> Int -> Int -> M Int
median x1 x2 x3 = do
  q <- get
  unless (q > 0) $ liftIO exitFailure
  ret <- liftIO $ do
    putStrLn $ intercalate " " (map show [x1, x2, x3])
    hFlush stdout
    readLn
  put (q - 1)
  return ret

sendSolution :: MonadIO m => [Int] -> m Bool
sendSolution xs = liftIO $ do
  putStrLn $ intercalate " " (map show xs)
  hFlush stdout
  ret <- readLn
  return $ ret == (1 :: Int)
