{-# OPTIONS -cpp #-}

import System.Directory
import System.Plugins
import Data.List
import PluginAPI
import Control.Applicative

loadPlugin :: FilePath -> IO PluginI
loadPlugin file = do res <- ((load_ file ["./"] "plugin") :: IO (LoadStatus PluginI))
                     case res of
                         LoadFailure msg -> error ("Failed to load " ++ file)
                         LoadSuccess _ v -> return v


loadMagicPlugins :: IO [PluginI]
loadMagicPlugins = do fileList <- getDirectoryContents "./"
                      let pluginFileList = filter (\x -> isSuffixOf ".o" x && x /= "PluginAPI.o" && x /= "Main.o") fileList
                      mapM loadPlugin pluginFileList

fromLoadSuc (LoadFailure _)   = error "load failed"
fromLoadSuc (LoadSuccess _ v) = v

files = ["file1", "file2", "file3"]

main = do
        plugins <- loadMagicPlugins
        let comparators = map (compareFiles) plugins
        results <- sequence [c a b | c <- comparators, a <- files, b <- files]
        print results
