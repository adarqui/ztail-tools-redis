{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, RecordWildCards #-}

{- Some code duplication.. done very fast to tie into wricardo's logios gui -}

import System.Environment

import ZTail
import Abstract.Wrapper
import Abstract.Queue.Enq
import Control.Monad
import Data.Aeson
import Data.Maybe
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import Prelude hiding (log)

data Logios = Logios {
 host :: String,
 file :: String,
 log :: String
} deriving (Show, Generic)

instance FromJSON Logios
instance ToJSON Logios

usage = "usage: ./ztail-enqueue <host-id-string> [<queue-url://>,..] <ztail-args...>"

tp'pack v = lazyToStrict $ encode v
tp'unpack v = fromJust $ (decode (strictToLazy v) :: Maybe Logios)

strictToLazy v = BSL.fromChunks [v]
lazyToStrict v = BS.concat $ BSL.toChunks v
encode'bsc v = lazyToStrict $ encode v

relay logios rqs _ TailPacket{..} = do
 mapM_ (\rq -> enqueue rq (logios { file = path, log = buf })) rqs

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (hid:urls:params) -> logger hid (read urls :: [String]) params
  _ -> error usage

logger hid urls params = do
 rqs <- mapM (\url -> mkQueue'Enq url tp'pack tp'unpack) urls
 let logios = Logios { host = hid }
 tails <- parse_args params
 run_main params tailText $ map (\t -> t { ioAct = relay logios rqs}) tails
