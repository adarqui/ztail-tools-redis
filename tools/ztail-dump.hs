module Main where

import ZTail.Tools.Dump
import ZTail.Tools.Common
import System.Environment

usage :: IO ()
usage = do
    putStrLn "usage: ./ztail-dump-redis <host> <output_directory>"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (redis_hosts:dir:[]) -> do
            dumpMain (splitRedisHosts redis_hosts) dir
        _ -> usage
