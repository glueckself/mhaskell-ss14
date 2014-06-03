{-# OPTIONS -cpp #-}

module Test (plugin) where

import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Crypto.Hash.MD5
import System.IO
import PluginAPI
import Control.Monad
import Data.Dynamic
import System.Directory
import System.FilePath

plugin = PluginI {
    magic = None,
    pluginName = "TestPlugin",
--    getFileRepresentation = getFileNameRep,
    compareFiles = compareNames,
    displayFile = error "boom"
}

compareNames :: FilePath -> FilePath -> IO Bool
compareNames a b = do return a == b

getFileNameRep :: FilePath -> IO String
getFileNameRep fp = do return fp
