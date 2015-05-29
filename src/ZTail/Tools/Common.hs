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
    redisHost
) where

import ZTail
import Data.Aeson
import Data.Maybe
import Data.List.Split

import Control.Exception
import Control.Concurrent

import qualified Database.Redis as Redis

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import GHC.Generics

data HostDataWrapper a = HostDataWrapper {
    h :: String,
    d :: a
} deriving (Show, Read, Generic)

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

defaultBoundedChanSize :: Int
defaultBoundedChanSize = 100

defaultThreadTimeout :: Int
defaultThreadTimeout = 5

defaultBLPopTimeout :: Integer
defaultBLPopTimeout = 30

defaultQueueName :: BS.ByteString
defaultQueueName = "ztail"

defaultPort :: Int
defaultPort = 60002

tp'pack :: ToJSON a => a -> BSC.ByteString
tp'pack v = lazyToStrict $ encode v

tp'unpack :: BSC.ByteString -> HostDataWrapper TailPacket
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe (HostDataWrapper TailPacket))

pack :: HostDataWrapper TailPacket -> BS.ByteString
pack tp = lazyToStrict $ encode $ tp

unpack :: BS.ByteString -> Maybe (HostDataWrapper TailPacket)
unpack v = fromJust $ decode $ strictToLazy v

unpack' :: BS.ByteString -> HostDataWrapper TailPacket
unpack' v = fromJust $ decode' $ strictToLazy v

strictToLazy :: BSC.ByteString -> BSL.ByteString
strictToLazy v = BSL.fromChunks [v]

lazyToStrict :: BSL.ByteString -> BSC.ByteString
lazyToStrict v = BS.concat $ BSL.toChunks v

encode'bsc :: ToJSON a => a -> BSC.ByteString
encode'bsc v = lazyToStrict $ encode v

safeConnect :: Redis.ConnectInfo -> (Redis.Connection -> IO ()) -> IO ()
safeConnect host cb = do
    catches
        (do
            bracket
                (do Redis.connect host)
                (\q -> Redis.runRedis q $ Redis.quit)
                (\q -> cb q)
        )
        [Handler someExceptionHandler, Handler redisExceptionHandler]
    where
        putErr e = putStrLn $ "safeConnect: " ++ show e
        someExceptionHandler :: SomeException -> IO ()
        someExceptionHandler e = putStrLn "SomeException" >> putErr e >> sleep 1
        redisExceptionHandler :: Redis.ConnectionLostException -> IO ()
        redisExceptionHandler e = putStrLn "ConnectionLostException" >> putErr e >> sleep 1

secToMsec n = n * 1000000

sleep :: Int -> IO ()
sleep n = threadDelay $ secToMsec n

splitRedisHosts :: String -> [Redis.ConnectInfo]
splitRedisHosts s = map redisHost $ splitOn "," s

redisHost :: String -> Redis.ConnectInfo
redisHost host = Redis.defaultConnectInfo { Redis.connectHost = host }
