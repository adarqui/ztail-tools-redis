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

data Enqueuer = Enqueuer {
    hostId :: String,
    redis :: Redis.ConnectInfo,
    ztailParams :: [String],
    ekg :: EKG
}

enqueueMain :: String -> Redis.ConnectInfo -> [String] -> IO ()
enqueueMain host_id host params = do
--    ekg'bootstrap defaultPort (enqueueMain' host_id host params)
    ekg'bootstrap defaultPort (runEnqueuer . newEnqueuer host_id host params)

newEnqueuer :: String -> Redis.ConnectInfo -> [String] -> EKG -> Enqueuer
newEnqueuer host_id host ztail_params ekg =
    Enqueuer {
        hostId = host_id,
        redis = host,
        ztailParams = ztail_params,
        ekg = ekg
    }

runEnqueuer :: Enqueuer -> IO ()
runEnqueuer enq@Enqueuer{..} = do
    -- Create a new bounded channel which caps out at defaultBoundedChanSize
    bc <- newBoundedChan defaultBoundedChanSize
    forkIO $ do
        forever $ do
            tails <- parse_args ztailParams
            run_main ztailParams tailText $
                map (\tail -> tail { ioAct = \_ tailPacket -> do
                    forkIO $ do
                        -- A timeout is necessary since every log entry creates it's
                        -- own thread & we are using a bounded channel. Without a timeout,
                        -- each thread would block as log entries start filling up. Instead,
                        -- we allow them to fill up the bounded channel,  block for the
                        -- specified timeout, then die if the -- system is overloaded
                        -- or unable to connect to redis.
                        timeout (secToMsec defaultThreadTimeout) $ writeChan bc tailPacket
                        putStrLn "done writing to chan"
                    return ()
                })
                tails
    forever $ do
        runSink enq bc

runSink :: Enqueuer -> BoundedChan TailPacket -> IO ()
runSink enq@Enqueuer{..} bc = do
    safeConnect redis $ \conn ->
        forever $ do
            tailPacket <- readChan bc
            putStrLn "GOT TP"
            relayTailPacket enq conn tailPacket

relayTailPacket :: Enqueuer -> Redis.Connection -> TailPacket -> IO ()
relayTailPacket enq@Enqueuer{..} conn tailPacket = do
    putStrLn "relay"
    catches
        (do relayTailPacket' enq conn tailPacket)
        [Handler someExceptionHandler, Handler redisExceptionHandler]
    where
        putErr e = putStrLn $ "relay: " ++ show e
        someExceptionHandler :: SomeException -> IO ()
        someExceptionHandler e = putStrLn "SomeException" >> putErr e >> sleep 1 >> relayTailPacket enq conn tailPacket
        redisExceptionHandler :: Redis.ConnectionLostException -> IO ()
        redisExceptionHandler e = putStrLn "ConnectionLost" >> sleep 1 >> relayTailPacket enq conn tailPacket

relayTailPacket' :: Enqueuer -> Redis.Connection -> TailPacket -> IO ()
relayTailPacket' enq@Enqueuer{..} conn tailPacket = do
    let
        tpWrapped = tp'pack $ HostDataWrapper { h = hostId, d = tailPacket }
        len = length $ buf tailPacket
    result <- Redis.runRedis conn $ do Redis.rpush defaultQueueName [tpWrapped]
    Counter.inc (_logCounter ekg)
    Gauge.add (_lengthGauge ekg) (fromIntegral len :: Int64)
    Distribution.add (_logDistribution ekg) (fromIntegral len :: Double)
