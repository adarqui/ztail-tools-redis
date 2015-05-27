{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Enqueue (
    enqueue_main
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

relay EKG{..} wrapper q _ tp = do
--    return ()
    putStrLn "relay!!"
    result <- Redis.runRedis q $ do Redis.rpush "ztail" [tp'pack (wrapper { d = tp })]
    -- stats
    let len = length $ buf tp
    Counter.inc _logCounter
    Gauge.add _lengthGauge (fromIntegral len :: Int64)
    Distribution.add _logDistribution (fromIntegral len :: Double)

enqueue_main :: String -> String -> [String] -> IO ()
enqueue_main host_id redis_host params = do
    putStrLn "enqueue_main"
    ekg'bootstrap port (main' host_id redis_host params)

main' :: String -> String -> [String] -> EKG -> IO ()
main' host_id redis_host params ekg = do
    putStrLn "main'"
    logger host_id redis_host params ekg

logger host_id redis_host params ekg = do
    putStrLn "logger"
    q <- Redis.connect Redis.defaultConnectInfo
    let wrapper = HostDataWrapper { h = host_id }
    tails <- parse_args params
    run_main params tailText $ map (\t -> t { ioAct = relay ekg wrapper q}) tails
