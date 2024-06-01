{-# LANGUAGE TypeApplications #-}
module DirSummary where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when, unless)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import Text.Printf (printf)

import Metrics

dropSuffix :: String -> String -> String
dropSuffix suffix s
    | suffix `isSuffixOf` s = take (length s - length suffix) s
    | otherwise = s

data FileType 
    = FileTypeDirectory
    | FileTypeRegular
    | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
    isDirectory <- doesDirectoryExist fname
    isFile <- doesFileExist fname
    pure $ case (isDirectory, isFile) of
                (True, False) -> FileTypeDirectory
                (False, True) -> FileTypeRegular
                _otherwise -> FileTypeOther

traverseDir :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDir metrics root action = do
    seenRef <- newIORef Set.empty
    let
        haveSeenDir canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef

        addDirToSeen canonicalPath =
             modifyIORef seenRef $ Set.insert canonicalPath

        handler ex = print ex >> tickFailure metrics

        traverseSubDir subdirPath =
            timeFunction metrics "traverseSubdirectory" $ do
                contents <- listDirectory subdirPath
                for_ contents $ \file' ->
                    handle @IOException handler $ do
                        let file = subdirPath <> "/" <> file'
                        canonicalPath <- canonicalizePath file
                        classification <- classifyFile canonicalPath
                        result <- case classification of
                            FileTypeOther -> pure ()
                            FileTypeRegular -> action file
                            FileTypeDirectory -> do
                                alreadyProcessed <- haveSeenDir file
                                unless alreadyProcessed $ do
                                    addDirToSeen file
                                    traverseSubDir file
                        tickSuccess metrics
                        pure result
    
    traverseSubDir (dropSuffix "/" root)

traverseDir' :: Metrics -> FilePath -> (FilePath -> a) -> IO [a]
traverseDir' metrics root action = do
    resultsRef <- newIORef []
    traverseDir metrics root $ \file -> do
        modifyIORef resultsRef (action file :)
    readIORef resultsRef

longestContents :: Metrics -> FilePath -> IO BS.ByteString
longestContents metrics root = do
    contentsRef <- newIORef BS.empty
    let
        takeLongestFile a b =
            if BS.length a >= BS.length b
            then a
            else b
        
    traverseDir metrics root $ \file -> do
        contents <- BS.readFile file
        modifyIORef contentsRef (takeLongestFile contents)

    readIORef contentsRef

dirSummaryWithMetrics :: FilePath -> IO ()
dirSummaryWithMetrics root = do
    metrics <- newMetrics
    histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
    traverseDir metrics root $ \file -> do
        putStrLn $ file <> ":"
        contents <- timeFunction metrics "TIO.readFile" $
            TIO.readFile file
        timeFunction metrics "wordcount" $
            let wordCount = length $ T.words contents
            in putStrLn $ "    word count: " <> show wordCount
        timeFunction metrics "histogram" $ do
            oldHistogram <- readIORef histogramRef
            let
                addCharToHist hist letter =
                    Map.insertWith (+) letter 1 hist
                newHist = T.foldl' addCharToHist oldHistogram contents
            writeIORef histogramRef newHist
    histogram <- readIORef histogramRef
    putStrLn "Histogram Data:"
    for_ (Map.toList histogram) $ \(letter, count) ->
        putStrLn $ printf "    %c: %d" letter count
    
    displayMetrics metrics
