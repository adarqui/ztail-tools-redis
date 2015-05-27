module Main where

import ZTail.Tools.Enqueue
import System.Environment

usage :: IO ()
usage = do
    putStrLn "usage: ./ztail-enqueue-redis <host-id-string> <redis-host>"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (host_id:redis_host:params) -> do
            enqueue_main host_id redis_host params
        _ -> usage
