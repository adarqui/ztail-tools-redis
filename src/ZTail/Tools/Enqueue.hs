{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Enqueue (
    enqueueMain
) where

import ZTail
import Control.Monad
import Data.Int

import Control.Exception
import Control.Concurrent hiding (Chan, getChanContents, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Concurrent.BoundedChan

import System.Timeout

import qualified Database.Redis as Redis

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

import ZTail.Tools.EKG
import ZTail.Tools.Common

enqueueMain :: String -> Redis.ConnectInfo -> [String] -> IO ()
enqueueMain host_id host params = do
    ekg'bootstrap defaultPort (enqueueMain' host_id host params)

enqueueMain' :: String -> Redis.ConnectInfo -> [String] -> EKG -> IO ()
enqueueMain' host_id host params ekg = do
    bc <- newBoundedChan defaultBoundedChanSize
    forkIO $ do
        forever $ do
            tails <- parse_args params
            run_main params tailText $
                map (\t -> t { ioAct = \_ tp -> do
                    forkIO $ do
                        -- A timeout is necessary since every log entry creates it's
                        -- own thread & we are using a bounded channel. Without a timeout,
                        -- each thread would block as log entries start filling up. Instead,
                        -- we allow them to fill up the bounded channel,  block for the
                        -- specified timeout, then die if the -- system is overloaded
                        -- or unable to connect to redis.
                        timeout (secToMsec defaultThreadTimeout) $ writeChan bc tp
                        putStrLn "done writing to chan"
                    return ()
                })
                tails
    forever $ do
        sink bc host_id host params ekg

sink bc host_id host params ekg = do
    let wrapper = HostDataWrapper { h = host_id, d = TailPacket{} }
    safeConnect host $ \q ->
        forever $ do
            tp <- readChan bc
            putStrLn "GOT TP"
            relay ekg wrapper q tp

relay ekg wrapper q tp = do
    putStrLn "relay"
    catches
        (do relay' ekg wrapper q tp)
        [Handler someExceptionHandler, Handler redisExceptionHandler]
    where
        putErr e = putStrLn $ "relay: " ++ show e
        someExceptionHandler :: SomeException -> IO ()
        someExceptionHandler e = putStrLn "SomeException" >> putErr e >> sleep 1 >> relay ekg wrapper q tp
        redisExceptionHandler :: Redis.ConnectionLostException -> IO ()
        redisExceptionHandler e = putStrLn "ConnectionLost" >> sleep 1 >> relay ekg wrapper q tp

relay' EKG{..} wrapper q tp = do
    let tp' = tp'pack $ wrapper { d = tp }
    result <- Redis.runRedis q $ do Redis.rpush defaultQueueName [tp']
    let len = length $ buf tp
    Counter.inc _logCounter
    Gauge.add _lengthGauge (fromIntegral len :: Int64)
    Distribution.add _logDistribution (fromIntegral len :: Double)
