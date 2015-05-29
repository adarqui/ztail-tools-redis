{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.Dump (
    dumpMain
) where

import ZTail
import Control.Monad
import Control.Concurrent.Async

import System.Directory
import System.FilePath.Posix

import qualified Database.Redis as Redis

import ZTail.Tools.EKG
import ZTail.Tools.Common

data Dumper = Dumper {
    redii :: [Redis.ConnectInfo],
    dir :: String,
    ekg :: EKG
}

newDumper :: [Redis.ConnectInfo] -> String -> EKG -> Dumper
newDumper redii dir ekg =
    Dumper {
        redii = redii,
        dir = dir,
        ekg = ekg
    }

dumpMain :: [Redis.ConnectInfo] -> String -> IO ()
dumpMain hosts dir = do
    ekg'bootstrap (defaultPort+1) (runDumper . newDumper hosts dir)

runDumper :: Dumper -> IO ()
runDumper dump@Dumper{..} = do
    mapM_ (\redis -> async $ dumpQueues dump redis) redii
    getLineLoop

dumpQueues :: Dumper -> Redis.ConnectInfo -> IO ()
dumpQueues Dumper{..} redis = do
    forever $ safeConnect redis $ \conn -> do
        forever $ do
            result <- Redis.runRedis conn $ Redis.blpop [defaultQueueName] defaultBLPopTimeout
            case result of
                (Left err) -> putStrLn (show err)
                (Right Nothing) -> return ()
                (Right (Just resultValue)) -> do
                    case (unpack (snd resultValue)) of
                        (Just tailPacket) -> do
                            let tailDir = (d tailPacket)
                            let buf' = (buf tailDir ++ "\n")
                            let logpath = (dir ++ "/" ++ (h tailPacket) ++ "/" ++ (path tailDir))
                            let basename = takeDirectory logpath
                            let len = length (buf tailDir)
                            createDirectoryIfMissing True basename
                            appendFile logpath buf'
                            updateSuccessfulEkgStats ekg len
                            return ()
                        Nothing -> return ()
