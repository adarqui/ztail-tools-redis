{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Dump (
    dump_main
) where

import System.Environment

import ZTail
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Data.Aeson
import Data.Maybe
import Data.Int

import System.Directory
import System.FilePath.Posix

import qualified Database.Redis as Redis

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Char8 as BSC

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as Monitoring

import ZTail.Tools.EKG
import ZTail.Tools.Common

dump'Queues redis_host EKG{..} Dump{..} = do
    q <- Redis.connect Redis.defaultConnectInfo
    forever $ do
        result <- Redis.runRedis q $ Redis.blpop ["ztail"] 30
        case result of
            (Left err) -> putStrLn (show err)
            (Right Nothing) -> putStrLn "nothing"
            (Right (Just tp)) -> do
                case (unpack (snd tp)) of
                    (Just tp') -> do
                        let d' = (d tp')
                        let buf' = (buf d' ++ "\n")
                        let logpath = (_dir ++ "/" ++ (h tp') ++ "/" ++ (path d'))
                        let basename = takeDirectory logpath
                        let len = length (buf d')
                        createDirectoryIfMissing True basename
                        appendFile logpath buf'
                        Counter.inc _logCounter
                        Gauge.add _lengthGauge (fromIntegral len :: Int64)
                        Distribution.add _logDistribution (fromIntegral len :: Double)
                        return ()
                    Nothing -> do
                        threadDelay 1000000

dump_main :: String -> String -> IO ()
dump_main redis_host dir = do
    ekg'bootstrap (port+1) (main' redis_host dir)

main' :: String -> String -> EKG -> IO ()
main' redis_host dir ekg = do
    dumper redis_host ekg dir redis_host >> return ()

dumper redis_host ekg dir urls = do
    let dump = Dump { _dir = dir, _all = dir ++ "/all.log" }
    dump'Queues redis_host ekg dump
    dumper redis_host ekg dir urls
