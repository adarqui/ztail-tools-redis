{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Enqueue (
    enqueueMain
) where

import ZTail
import Control.Monad
import Data.Int

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Database.Redis as Redis

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

import ZTail.Tools.EKG
import ZTail.Tools.Common

enqueueMain :: String -> Redis.ConnectInfo -> [String] -> IO ()
enqueueMain host_id host params = do
    ekg'bootstrap port (enqueueMain' host_id host params)

enqueueMain' :: String -> Redis.ConnectInfo -> [String] -> EKG -> IO ()
enqueueMain' host_id host params ekg = do
    mv <- newEmptyMVar
    forkIO $ do
        forever $ do
            tails <- parse_args params
            run_main params tailText $ map (\t -> t { ioAct = \_ tp -> (putStrLn (show tp) >> putMVar mv tp) }) tails
    forever $ do
        sink mv host_id host params ekg

sink mv host_id host params ekg = do
    let wrapper = HostDataWrapper { h = host_id, d = "" }
    safeConnect host $ \q -> do
        forever $ do
            tp <- takeMVar mv
            relay ekg wrapper q tp

relay EKG{..} wrapper q tp = do
    result <- Redis.runRedis q $ do Redis.rpush "ztail" [tp'pack (wrapper { d = tp })]
    let len = length $ buf tp
    Counter.inc _logCounter
    Gauge.add _lengthGauge (fromIntegral len :: Int64)
    Distribution.add _logDistribution (fromIntegral len :: Double)
