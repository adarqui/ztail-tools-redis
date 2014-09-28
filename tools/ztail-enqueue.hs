{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import ZTail
import Abstract.Queue.Enq

import qualified Data.ByteString.Char8 as B

usage = "usage: ./ztail-enqueue <queue-url://> <ztail-args...>"

relay rq _ tp = do
 enqueue rq (show tp)

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (url:params) -> logger url params
  _ -> error usage

logger url params = do
 rq <- mkQueue'Enq url
 tails <- parse_args params
 run_main params tailText $ map (\t -> t { ioAct = relay rq}) tails
