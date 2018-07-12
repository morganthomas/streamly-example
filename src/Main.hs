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


testData :: [Int]
testData = [1,2,3,4]


testStream :: IO (SerialT IO Int)
testStream = do
  out <- newEmptyMVar
  forkIO $ go testData out
  return . serially $ S.repeatM (takeMVar out)

  where
    go []     out = return ()
    go (x:xs) out = do
      threadDelay 1000000
      putMVar out x
      go xs out


main :: IO ()
main = do
  s <- testStream
  void . forkIO . printStream . adapt $ fmap ("R",) s
  void . forkIO . printStream . adapt $ fmap ("G",) s
  void . forkIO . printStream . adapt $ fmap ("B",) s
  threadDelay 5000000


printStream :: Show a => SerialT IO a -> IO ()
printStream = S.mapM_ (liftIO . putStrLn . show)
