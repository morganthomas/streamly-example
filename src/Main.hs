{-# LANGUAGE TupleSections, FlexibleContexts #-}


module Main where


import Control.Monad.Catch         (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import qualified Control.Foldl as L
import qualified Data.Map         as M
import Streamly
import Streamly.Prelude as S
import UnliftIO
import UnliftIO.Concurrent


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
  [sR,sG,sB] <- testStream >>= cloneStream 3
  void . forkIO . printStream . adapt $ fmap ("R",) sR
  void . forkIO . printStream . adapt $ fmap ("G",) sG
  void . forkIO . printStream . adapt $ fmap ("B",) sB
  threadDelay 5000000


printStream :: Show a => SerialT IO a -> IO ()
printStream = S.mapM_ (liftIO . putStrLn . show)


-- A stream of actions should be considered single-consumer because each action result
-- will be given to one consumer only. With cloneStream you can create multiple distinct
-- copies of a stream to pass to multiple consumers.
cloneStream :: (MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m)
  => Int -> SerialT m a ->  m [SerialT m a]
cloneStream n s = do
  vars <- Prelude.sequence (replicate n newEmptyMVar)
  void . forkIO $ S.mapM_ (\x -> forM_ vars (\v -> putMVar v x)) s
  return $ fmap (\v -> serially . S.repeatM $ takeMVar v) vars
