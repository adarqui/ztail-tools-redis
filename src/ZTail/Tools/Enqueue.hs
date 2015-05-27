{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Enqueue (
    enqueueMain
) where

import System.Environment

import ZTail
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Int

import qualified Database.Redis as Redis

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as Monitoring

import ZTail.Tools.EKG
import ZTail.Tools.Common

enqueueMain :: String -> Redis.ConnectInfo -> [String] -> IO ()
enqueueMain host_id host params = do
    putStrLn "enqueue_main"
    ekg'bootstrap port (enqueueMain' host_id host params)

enqueueMain' :: String -> Redis.ConnectInfo -> [String] -> EKG -> IO ()
enqueueMain' host_id host params ekg = do
    putStrLn "main'"
    forever $ do
        logger host_id host params ekg

logger host_id host params ekg = do
    putStrLn "logger"
    safeConnect host $ \q -> do
        let wrapper = HostDataWrapper { h = host_id }
        tails <- parse_args params
        run_main params tailText $ map (\t -> t { ioAct = relay ekg wrapper q}) tails

relay EKG{..} wrapper q _ tp = do
    putStrLn "relay!!"
    result <- Redis.runRedis q $ do Redis.rpush "ztail" [tp'pack (wrapper { d = tp })]
    let len = length $ buf tp
    Counter.inc _logCounter
    Gauge.add _lengthGauge (fromIntegral len :: Int64)
    Distribution.add _logDistribution (fromIntegral len :: Double)
