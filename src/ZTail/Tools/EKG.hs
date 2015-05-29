{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZTail.Tools.EKG (
    EKG(..),
    getLineLoop,
    ekg'bootstrap,
    updateSuccessfulEkgStats,
    updateFailureEkgStats,
    timed,
    getTime
) where

import Control.Monad
import Data.List
import Data.Int
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment
import qualified Data.Text as T
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as Monitoring

data EKG = EKG {
    _server :: Monitoring.Server,
    _logCounter :: Counter.Counter,
    _errorCounter :: Counter.Counter,
    _logDistribution :: Distribution.Distribution,
    _lengthGauge :: Gauge.Gauge
}

getLineLoop :: IO ()
getLineLoop = forever $ getLine

ekg'bootstrap :: Int -> (EKG -> IO b) -> IO b
ekg'bootstrap port io = do
    argv <- getArgs
    handle <- Monitoring.forkServer "0.0.0.0" port
    logCounter <- Monitoring.getCounter "log_counter" handle
    errorCounter <- Monitoring.getCounter "error_counter" handle
    logDistribution <- Monitoring.getDistribution "log_distribution" handle
    lengthGauge <- Monitoring.getGauge "length_gauge" handle
    label <- Monitoring.getLabel "argv" handle
    Label.set label $ T.pack $ concat $ intersperse " " argv
    let ekg = EKG {
        _server = handle,
        _logCounter = logCounter,
        _logDistribution = logDistribution,
        _lengthGauge = lengthGauge,
        _errorCounter = errorCounter
    }
    io ekg

-- Update the following successful ekg stats:
-- log_counter
-- log_distribution
-- length_gauge
updateSuccessfulEkgStats :: EKG -> Int -> IO ()
updateSuccessfulEkgStats EKG{..} len = do
    Counter.inc _logCounter
    Gauge.add _lengthGauge (fromIntegral len :: Int64)
    Distribution.add _logDistribution (fromIntegral len :: Double)

-- Update the following failure ekg stats:
-- error_counter
updateFailureEkgStats :: EKG -> IO ()
updateFailureEkgStats EKG{..} = do
    Counter.inc _errorCounter

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    _ <- m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
