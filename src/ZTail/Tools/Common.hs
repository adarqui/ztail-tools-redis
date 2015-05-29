{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Common (
    HostDataWrapper (..),
    defaultQueueName,
    defaultPort,
    defaultBoundedChanSize,
    defaultThreadTimeout,
    defaultBLPopTimeout,
    tp'pack,
    tp'unpack,
    pack,
    unpack,
    unpack',
    strictToLazy,
    lazyToStrict,
    encode'bsc,
    safeConnect,
    secToMsec,
    sleep,
    splitRedisHosts,
    redisHost,
    logOut,
    logErr
) where

import ZTail
import Data.Aeson
import Data.Maybe
import Data.List.Split

import System.IO
import Network

import Control.Exception
import Control.Concurrent

import qualified Database.Redis as Redis

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import GHC.Generics

import Prelude hiding (log)

data HostDataWrapper = HostDataWrapper {
    h :: String,
    d :: TailPacket
} deriving (Show, Read, Generic)

instance FromJSON HostDataWrapper
instance ToJSON HostDataWrapper

defaultBoundedChanSize :: Int
defaultBoundedChanSize = 1024

defaultThreadTimeout :: Int
defaultThreadTimeout = 5

defaultBLPopTimeout :: Integer
defaultBLPopTimeout = 30

defaultQueueName :: BS.ByteString
defaultQueueName = "ztail"

defaultPort :: Int
defaultPort = 60002

-- begin crazyness
tp'pack :: ToJSON a => a -> BSC.ByteString
tp'pack v = lazyToStrict $ encode v

tp'unpack :: BSC.ByteString -> HostDataWrapper
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe HostDataWrapper)

pack :: HostDataWrapper -> BS.ByteString
pack tp = lazyToStrict $ encode $ tp

unpack :: BS.ByteString -> Maybe HostDataWrapper
unpack v = fromJust $ decode $ strictToLazy v

unpack' :: BS.ByteString -> HostDataWrapper
unpack' v = fromJust $ decode' $ strictToLazy v

strictToLazy :: BSC.ByteString -> BSL.ByteString
strictToLazy v = BSL.fromChunks [v]

lazyToStrict :: BSL.ByteString -> BSC.ByteString
lazyToStrict v = BS.concat $ BSL.toChunks v

encode'bsc :: ToJSON a => a -> BSC.ByteString
encode'bsc v = lazyToStrict $ encode v
-- end crazyness

safeConnect :: Redis.ConnectInfo -> (Redis.Connection -> IO ()) -> IO ()
safeConnect host cb = do
    bracketOnError
        (logOut "Connecting.." >> Redis.connect host)
        (\_ -> logErr "Disconnected." >> sleep 1 >> safeConnect host cb)
        (\conn -> logOut "Connected." >> cb conn)

secToMsec :: Int -> Int
secToMsec n = n * 1000000

sleep :: Int -> IO ()
sleep n = threadDelay $ secToMsec n

splitRedisHosts :: String -> [Redis.ConnectInfo]
splitRedisHosts s = map redisHost $ splitOn "," s

redisHost :: String -> Redis.ConnectInfo
redisHost host =
    case (splitOn ":" host) of
        [] -> Redis.defaultConnectInfo { Redis.connectHost = host }
        (host':[]) -> Redis.defaultConnectInfo { Redis.connectHost = host' }
        (host':port':[]) -> Redis.defaultConnectInfo { Redis.connectHost = host', Redis.connectPort = (PortNumber (fromIntegral (read port' :: Int))) }
        _ -> error "Invalid host specification."

logErr :: String -> IO ()
logErr msg = hPutStrLn stderr $ "ztail: " ++ msg

logOut :: String -> IO ()
logOut msg = putStrLn $ "ztail: " ++ msg
