{-# LANGUAGE RecordWildCards #-}
module Metrics where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.Time.Clock
    ( diffUTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    )


data AppMetrics = AppMetrics
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: Map.Map String Int
    } deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

newMetrics :: IO Metrics
newMetrics =
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

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    result    <- action
    endTime   <- getCurrentTime

    modifyIORef metrics $ \oldMetrics ->
        let
            oldDurationVal =
                fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

            runDuration =
                floor . nominalDiffTimeToSeconds $
                    diffUTCTime endTime startTime

            newDurationVal = oldDurationVal + runDuration
            
        in oldMetrics { 
            callDuration = Map.insert actionName newDurationVal $ 
                callDuration oldMetrics
            }
    
    pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
    AppMetrics{..} <- readIORef metricsStore
    putStrLn $ "successes: " <> show successCount
    putStrLn $ "failures: " <> show failureCount
    for_ (Map.toList callDuration) $ \(functionName, timing) ->
        putStrLn $ printf "Time spent in \"%s\": $d" functionName timing

printMetrics :: IO ()
printMetrics =
    appMetricsStore <$> newMetrics >>= readIORef >>= print

incSuccess :: IO ()
incSuccess =
    appMetricsStore <$> newMetrics >>= flip modifyIORef incSuccess
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
