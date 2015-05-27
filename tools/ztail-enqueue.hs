module Main where

import ZTail.Tools.Enqueue
import ZTail.Tools.Common
import System.Environment

usage :: IO ()
usage = do
    putStrLn "usage: ./ztail-enqueue-redis <host-id-string> <redis-host>"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (host_id:redis_host:params) -> do
            enqueueMain host_id (redisHost redis_host) params
        _ -> usage
