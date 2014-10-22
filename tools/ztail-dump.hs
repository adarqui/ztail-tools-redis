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

import System.Directory
import System.FilePath.Posix

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Char8 as BSC

data Dump = Dump {
 _dir :: String,
 _all :: String
} deriving (Show, Read)

instance FromJSON (HostDataWrapper TailPacket)
instance ToJSON (HostDataWrapper TailPacket)

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

dump'Queues Dump{..} rq = do
 forever $ do
  tp <- blDequeue rq
  case tp of
   (Just tp') -> do
    let d' = (d tp')
    let buf' = (buf d' ++ "\n")
    let logpath = (_dir ++ "/" ++ (h tp') ++ "/" ++ (path d'))
    let basename = takeDirectory logpath
--    appendFile _all $ show tp'
    createDirectoryIfMissing True basename
    appendFile logpath buf'
    return ()
   Nothing -> threadDelay 1000000

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (dir:urls:[]) -> dumper dir (read urls :: [String])
  _ -> error usage

dumper dir urls = do
 rqs <- mapM (\url -> mkQueue url pack' unpack') urls
 let dump = Dump { _dir = dir, _all = dir ++ "/all.log" }
 mapM_ (\rq -> forkIO $ dump'Queues dump rq) rqs
 forever $ getLine >> putStrLn "Hi."
