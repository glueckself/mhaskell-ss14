{-# OPTIONS -fglasgow-exts #-}
-- {-# LANGUAGE ExistentialQuantification #-}

module PluginAPI where

import Data.ByteString
import System.IO
import Data.Typeable

data MagicType = None | Sequence ByteString Int

data PluginI = PluginI {
    magic :: MagicType,
    pluginName :: String,
--    getFileRepresentation :: FilePath -> IO String,
    compareFiles :: FilePath -> FilePath -> IO Bool,
    displayFile :: FilePath -> IO ()
} deriving Typeable
