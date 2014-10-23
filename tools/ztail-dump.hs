{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, RecordWildCards #-}

{- FIXME: fuck man.. this should be mkQueue'Deq not mkQueue.. having problems compiling, cabal dependency hell wreckage -}

import System.Environment

import ZTail
import Abstract.Wrapper
import Abstract.Queue
import Abstract.Interfaces.Queue
import Control.Monad
import Control.Concurrent
import Data.Aeson
import Data.Maybe
import Data.Int

import System.Directory
import System.FilePath.Posix

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Char8 as BSC

import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as Monitoring

import ZTail.Tools.EKG

data Dump = Dump {
 _dir :: String,
 _all :: String
} deriving (Show, Read)

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

port = 60000

usage = "usage: ./ztail-dump <dir> [<queue-url://>,..]"

tp'pack v = lazyToStrict $ encode v
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe (HostDataWrapper TailPacket))

strictToLazy v = BSL.fromChunks [v]
lazyToStrict v = BS.concat $ BSL.toChunks v
encode'bsc v = lazyToStrict $ encode v

pack' :: HostDataWrapper TailPacket -> BS.ByteString
pack' tp = lazyToStrict $ encode $ tp

unpack' :: BS.ByteString -> HostDataWrapper TailPacket
unpack' v = fromJust $ decode' $ strictToLazy v

dump'Queues EKG{..} Dump{..} rq = do
 forever $ do
  tp <- blDequeue rq
  case tp of
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
    Counter.inc _dequeueErrorCounter
    threadDelay 1000000

main :: IO ()
main = do
 ekg'bootstrap port main'

main' :: EKG -> IO ()
main' ekg = do
 argv <- getArgs
 case argv of
  (dir:urls:[]) -> dumper ekg dir (read urls :: [String])
  _ -> error usage

dumper ekg dir urls = do
 rqs <- mapM (\url -> mkQueue url pack' unpack') urls
 let dump = Dump { _dir = dir, _all = dir ++ "/all.log" }
 mapM_ (\rq -> forkIO $ dump'Queues ekg dump rq) rqs
