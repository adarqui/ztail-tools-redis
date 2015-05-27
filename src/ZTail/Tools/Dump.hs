{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Dump (
    dumpMain
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

dumpMain :: [Redis.ConnectInfo] -> String -> IO ()
dumpMain hosts dir = do
    putStrLn "WTFFFFFFFFFFFFFFFFFFF"
    mapM_ (putStrLn . Redis.connectHost) hosts
    ekg'bootstrap (port+1) (dumpMain' hosts dir)

dumpMain' :: [Redis.ConnectInfo] -> String -> EKG -> IO ()
dumpMain' hosts dir ekg = do
    forever $ do
        dumper hosts ekg dir
        putStrLn "sleep"

dumper hosts ekg dir = do
    putStrLn "dumper"
    let dump = Dump { _dir = dir, _all = dir ++ "/all.log" }
    threads <- mapM (\host' -> async $ dump'Queues host' ekg dump) hosts
    waitAnyCancel threads

dump'Queues hosts EKG{..} Dump{..} = do
    putStrLn "dump'Queues"
    safeConnect hosts $ \q -> do
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
                            sleep 1
