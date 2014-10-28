{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, RecordWildCards #-}

import System.Environment

import ZTail
import Abstract.Wrapper
import Abstract.Queue.Enq
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Int

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as Monitoring

import ZTail.Tools.EKG

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

port = 60002

usage = "usage: ./ztail-enqueue <host-id-string> [<queue-url://>,..] <ztail-args...>"

tp'pack v = lazyToStrict $ encode v
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe (HostDataWrapper TailPacket))

strictToLazy v = BSL.fromChunks [v]
lazyToStrict v = BS.concat $ BSL.toChunks v
encode'bsc v = lazyToStrict $ encode v

relay EKG{..} wrapper rqs _ tp = do
 mapM_ (\rq -> enqueue rq (wrapper { d = tp })) rqs
 -- stats
 let len = length $ buf tp
 Counter.inc _logCounter
 Gauge.add _lengthGauge (fromIntegral len :: Int64)
 Distribution.add _logDistribution (fromIntegral len :: Double)

main :: IO ()
main = do
 ekg'bootstrap port main'

main' :: EKG -> IO ()
main' ekg = do
 argv <- getArgs
 case argv of
  (hid:urls:params) -> logger ekg hid (read urls :: [String]) params
  _ -> error usage

logger ekg hid urls params = do
 rqs <- mapM (\url -> mkQueue'Enq url tp'pack tp'unpack) urls
 let wrapper = HostDataWrapper { h = hid }
 tails <- parse_args params
 run_main params tailText $ map (\t -> t { ioAct = relay ekg wrapper rqs}) tails
