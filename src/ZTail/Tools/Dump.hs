{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Dump (
    dumpMain
) where

import ZTail
import Control.Monad
import Control.Concurrent.Async
import Data.Int

import System.Directory
import System.FilePath.Posix

import qualified Database.Redis as Redis

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

import ZTail.Tools.EKG
import ZTail.Tools.Common

data Dump = Dump {
    _dir :: String,
    _all :: String
} deriving (Show, Read)

dumpMain :: [Redis.ConnectInfo] -> String -> IO ()
dumpMain hosts dir = do
    ekg'bootstrap (port+1) (dumpMain' hosts dir)

dumpMain' :: [Redis.ConnectInfo] -> String -> EKG -> IO ()
dumpMain' hosts dir ekg = do
    forever $ do
        dumper hosts ekg dir

dumper :: [Redis.ConnectInfo] -> EKG -> String -> IO ()
dumper hosts ekg dir = do
    let dump = Dump { _dir = dir, _all = dir ++ "/all.log" }
    threads <- mapM (\host' -> async $ dump'Queues host' ekg dump) hosts
    _ <- waitAnyCancel threads
    return ()

dump'Queues :: Redis.ConnectInfo -> EKG -> Dump -> IO ()
dump'Queues host EKG{..} Dump{..} = do
    safeConnect host $ \q -> do
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
