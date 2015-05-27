module Main where

import ZTail.Tools.Dump
import System.Environment

usage :: IO ()
usage = do
    putStrLn "usage: ./ztail-dump-redis <redis-host> <dir>"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (redis_host:dir:[]) -> do
            dump_main redis_host dir
        _ -> usage
