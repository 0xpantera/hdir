{-# LANGUAGE TypeApplications #-}
module MyLib where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
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

