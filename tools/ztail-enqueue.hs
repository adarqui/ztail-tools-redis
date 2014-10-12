{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances #-}

import System.Environment

import ZTail
import Abstract.Wrapper
import Abstract.Queue.Enq
import Control.Monad
import Data.Aeson
import Data.Maybe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

usage = "usage: ./ztail-enqueue <host-id-string> [<queue-url://>,..] <ztail-args...>"

tp'pack v = lazyToStrict $ encode v
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe (HostDataWrapper TailPacket))

strictToLazy v = BSL.fromChunks [v]
lazyToStrict v = BS.concat $ BSL.toChunks v
encode'bsc v = lazyToStrict $ encode v

relay wrapper rqs _ tp = do
 mapM_ (\rq -> enqueue rq (wrapper { d = tp })) rqs

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (hid:urls:params) -> logger hid (read urls :: [String]) params
  _ -> error usage

logger hid urls params = do
 rqs <- mapM (\url -> mkQueue'Enq url tp'pack tp'unpack) urls
 let wrapper = HostDataWrapper { h = hid }
 tails <- parse_args params
 run_main params tailText $ map (\t -> t { ioAct = relay wrapper rqs}) tails
