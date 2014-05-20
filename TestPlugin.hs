{-# OPTIONS -cpp #-}

module Test (plugin) where

import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Crypto.Hash.MD5
import System.IO
import PluginAPI
import Control.Monad
import Data.Dynamic

plugin = PluginI {
    magic = None,
    pluginName = "TestPlugin",
    compareFiles = error "boom",
    displayFile = error "boom"
}
