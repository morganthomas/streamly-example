{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Functor
import qualified Data.Map         as M
import Streamly
import Streamly.Prelude as S

testData :: [(Int, Int)]
testData = [(0,1),(1,1),(2,1), (0,2),(1,2),(2,2), (0,3),(1,3),(2,3)]

testSubstream :: Int -> IO (AsyncT IO Int)
testSubstream i = do
  wrap <- newEmptyMVar
  void . forkIO . putMVar wrap . S.mapM (\(_,j) -> threadDelay (5000 * j + 2000 * i) >> return j)
    . S.fromFoldable . Prelude.filter ((== i) . fst) $ testData
  takeMVar wrap


asyncStream :: IO (AsyncT IO (Int, Int))
asyncStream =
  forConcurrently [0,1,2] (\i -> testSubstream i >>= return . fmap (i,)) >>= return . mconcat

testStream :: IO (AsyncT IO (M.Map Int Int))
testStream = asyncStream >>= return . collectMap

main :: IO ()
main = testStream >>= printStream . adapt

printStream :: Show a => SerialT IO a -> IO ()
printStream = S.mapM_ (liftIO . putStrLn . show)

collectMap :: (IsStream t, Ord k, Monad m) => t m (k,v) -> t m (M.Map k v)
collectMap = S.scanl' (\a (k,v) -> M.insert k v a) mempty
