{-# LANGUAGE TypeApplications #-}
module MyLib where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when, unless)
import Data.Foldable (for_)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import qualified Data.Set as Set (empty, insert, member)
import Text.Printf (printf)

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

traverseDir :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseDir root action = do
    seenRef <- newIORef Set.empty
    let
        haveSeenDir canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef

        addDirToSeen canonicalPath =
             modifyIORef seenRef $ Set.insert canonicalPath

        traverseSubDir subdirPath = do
            contents <- listDirectory subdirPath
            for_ contents $ \file' ->
                handle @IOException (\_ -> pure ()) $ do
                    let file = subdirPath <> "/" <> file'
                    canonicalPath <- canonicalizePath file
                    classification <- classifyFile canonicalPath
                    case classification of
                        FileTypeOther -> pure ()
                        FileTypeRegular -> action file
                        FileTypeDirectory -> do
                            alreadyProcessed <- haveSeenDir file
                            unless alreadyProcessed $ do
                                addDirToSeen file
                                traverseSubDir file
    
    traverseSubDir (dropSuffix "/" root)


traverseDir' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDir' root action = do
    resultsRef <- newIORef []
    traverseDir root $ \file -> do
        modifyIORef resultsRef (action file :)
    readIORef resultsRef

longestContents :: FilePath -> IO BS.ByteString
longestContents root = do
    contentsRef <- newIORef BS.empty
    let
        takeLongestFile a b =
            if BS.length a >= BS.length b
            then a
            else b
        
    traverseDir root $ \file -> do
        contents <- BS.readFile file
        modifyIORef contentsRef (takeLongestFile contents)

    readIORef contentsRef