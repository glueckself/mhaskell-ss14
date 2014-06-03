{-# OPTIONS -cpp #-}

module MD5Compare (plugin) where

import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Crypto.Hash.MD5
import System.IO
import PluginAPI
import Control.Monad
import Data.Dynamic
import Strings as S

plugin = PluginI {
    magic = None,
    pluginName = "MD5 Compare",
    getFileRepresentation = (liftM $ S.b2s) . hashFile,
    displayFile = printFileName
}

hashFile :: FilePath -> IO BS.ByteString
hashFile = liftM hashlazy . BSL.readFile

compareMd5 :: FilePath -> FilePath -> IO Bool
compareMd5 fp1 fp2 = liftM2 (==) (hashFile fp1) (hashFile fp2)

printFileName :: FilePath -> IO ()
printFileName fp = putStrLn (show fp)
