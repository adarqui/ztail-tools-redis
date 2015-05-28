{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Common (
    HostDataWrapper (..),
    tp'pack,
    tp'unpack,
    pack,
    unpack,
    unpack',
    strictToLazy,
    lazyToStrict,
    encode'bsc,
    port,
    safeConnect,
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

port :: Int
port = 60002

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
                (do Redis.connect Redis.defaultConnectInfo)
                (\q -> Redis.runRedis q $ Redis.quit)
                (\q -> cb q)
        )
        [Handler someExceptionHandler, Handler redisExceptionHandler]
    where
        putErr e = putStrLn $ "connectEndpoints: " ++ show e
        someExceptionHandler :: SomeException -> IO ()
        someExceptionHandler e = putErr e >> sleep 1
        redisExceptionHandler :: Redis.ConnectionLostException -> IO ()
        redisExceptionHandler e = putErr e >> sleep 1

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)

splitRedisHosts :: String -> [Redis.ConnectInfo]
splitRedisHosts s = map redisHost $ splitOn "," s

redisHost :: String -> Redis.ConnectInfo
redisHost host = Redis.defaultConnectInfo { Redis.connectHost = host }
