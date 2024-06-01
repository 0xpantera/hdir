module Metrics where

import qualified Data.Map.Strict as Map
import Data.IORef

data AppMetrics = AppMetrics
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: Map.Map String Int
    } deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

metrics' :: IO Metrics
metrics' =
    let
        emptyAppMetrics = AppMetrics
            { successCount = 0
            , failureCount = 0
            , callDuration = Map.empty
            }
    in Metrics <$> newIORef emptyAppMetrics

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
    m { successCount = 1 + successCount m }

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
    m { failureCount = 1 + failureCount m }

printMetrics :: IO ()
printMetrics =
    metrics' >>= readIORef >>= print

incSuccess :: IO ()
incSuccess =
    metrics' >>= flip modifyIORef incSuccess
    where
        incSuccess m =
            m { successCount = 1 + successCount m}

successfullyPrintHello :: IO ()
successfullyPrintHello = do
    print "Hello"
    incSuccess

printHelloAndMetrics :: IO ()
printHelloAndMetrics = do
    successfullyPrintHello
    printMetrics
