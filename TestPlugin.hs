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

plugin = PluginI {
    magic = None,
    pluginName = "TestPlugin",
    compareFiles = compareNames,
    displayFile = error "boom"
}

compareNames :: FilePath -> FilePath -> IO Bool
compareNames p1 p2 = do return (p1 == p2)
                        
