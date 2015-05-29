{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Enqueue (
    enqueueMain
) where

import ZTail
import Control.Monad

import Control.Exception
import Control.Concurrent hiding (Chan, getChanContents, readChan, writeChan)
import Control.Concurrent.BoundedChan

import System.Timeout

import qualified Database.Redis as Redis

import ZTail.Tools.EKG
import ZTail.Tools.Common

data Enqueuer = Enqueuer {
    hostId :: String,
    redis :: Redis.ConnectInfo,
    ztailParams :: [String],
    ekg :: EKG
}

newEnqueuer :: String -> Redis.ConnectInfo -> [String] -> EKG -> Enqueuer
newEnqueuer host_id host ztail_params ekg =
    Enqueuer {
        hostId = host_id,
        redis = host,
        ztailParams = ztail_params,
        ekg = ekg
    }

enqueueMain :: String -> Redis.ConnectInfo -> [String] -> IO ()
enqueueMain host_id host params = do
    ekg'bootstrap defaultPort (runEnqueuer . newEnqueuer host_id host params)

runEnqueuer :: Enqueuer -> IO ()
runEnqueuer enq@Enqueuer{..} = do
    -- Create a new bounded channel which caps out at defaultBoundedChanSize
    bc <- newBoundedChan defaultBoundedChanSize
    _ <- forkIO $ do
        forever $ do
            ztails <- parse_args ztailParams
            run_main ztailParams tailText $
                map (\ztail -> ztail { ioAct = \_ ztailPacket -> do
                    _ <- forkIO $ do
                        -- A timeout is necessary since every log entry creates it's
                        -- own thread & we are using a bounded channel. Without a timeout,
                        -- each thread would block as log entries start filling up. Instead,
                        -- we allow them to fill up the bounded channel,  block for the
                        -- specified timeout, then die if the -- system is overloaded
                        -- or unable to connect to redis.
                        result <- timeout (secToMsec defaultThreadTimeout) $ writeChan bc ztailPacket
                        case result of
                            Nothing -> updateFailureEkgStats ekg
                            (Just _) -> return ()
                    return ()
                })
                ztails
    forever $ do
        runSink enq bc
    return ()

runSink :: Enqueuer -> BoundedChan TailPacket -> IO ()
runSink enq@Enqueuer{..} bc = do
    safeConnect redis $ \conn ->
        forever $ do
            ztailPacket <- readChan bc
            relayTailPacket enq conn ztailPacket

relayTailPacket :: Enqueuer -> Redis.Connection -> TailPacket -> IO ()
relayTailPacket enq@Enqueuer{..} conn ztailPacket = do
    onException (do relayTailPacket' enq conn ztailPacket) (do sleep 1 >> relayTailPacket enq conn ztailPacket)

relayTailPacket' :: Enqueuer -> Redis.Connection -> TailPacket -> IO ()
relayTailPacket' Enqueuer{..} conn ztailPacket = do
    let
        ztpWrapped = tp'pack $ HostDataWrapper { h = hostId, d = ztailPacket }
        len = length $ buf ztailPacket
    _ <- Redis.runRedis conn $ do Redis.rpush defaultQueueName [ztpWrapped]
    updateSuccessfulEkgStats ekg len
