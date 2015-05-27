{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Common (
    HostDataWrapper (..),
    Dump (..),
    tp'pack,
    tp'unpack,
    pack,
    unpack,
    unpack',
    strictToLazy,
    lazyToStrict,
    encode'bsc,
    port
) where

import ZTail
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Int

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import GHC.Generics

data Dump = Dump {
 _dir :: String,
 _all :: String
} deriving (Show, Read)

data HostDataWrapper a = HostDataWrapper {
    h :: String,
    d :: a
} deriving (Show, Read, Generic)

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

port :: Int
port = 60002

tp'pack v = lazyToStrict $ encode v
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe (HostDataWrapper TailPacket))

pack :: HostDataWrapper TailPacket -> BS.ByteString
pack tp = lazyToStrict $ encode $ tp

unpack :: BS.ByteString -> Maybe (HostDataWrapper TailPacket)
unpack v = fromJust $ decode $ strictToLazy v

unpack' :: BS.ByteString -> HostDataWrapper TailPacket
unpack' v = fromJust $ decode' $ strictToLazy v

strictToLazy v = BSL.fromChunks [v]
lazyToStrict v = BS.concat $ BSL.toChunks v
encode'bsc v = lazyToStrict $ encode v
