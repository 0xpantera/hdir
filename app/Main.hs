module Main where

import System.Environment (getArgs)

import DirSummary (dirSummaryWithMetrics)


main :: IO ()
main = getArgs >>= dirSummaryWithMetrics . head
