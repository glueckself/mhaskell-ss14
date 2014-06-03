{-# OPTIONS -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import System.Directory
import System.Plugins
import Data.List
import PluginAPI
import Control.Applicative
import Pipes.Safe (Base, MonadSafe, bracket)
import Control.Monad (unless, when)
import Pipes (lift, every, runEffect, for)
import Pipes.Safe (bracket, runSafeP)
import Strings (b2s)
import System.Posix.FilePath ((</>))
import Sensing
import Numeric

debug_fp = False

internalObjects = ["PluginAPI.o", "Main.o", "Sensing.o", "Strings.o"]

loadPlugin :: FilePath -> IO PluginI
loadPlugin file = do res <- ((load_ file ["./"] "plugin") :: IO (LoadStatus PluginI))
                     case res of
                         LoadFailure msg -> error ("Failed to load " ++ file)
                         LoadSuccess _ v -> return v


loadMagicPlugins :: IO [PluginI]
loadMagicPlugins = do fileList <- getDirectoryContents "./"
                      let pluginFileList = filter (\x -> isSuffixOf ".o" x && notElem x internalObjects) fileList
                      mapM loadPlugin pluginFileList

fromLoadSuc (LoadFailure _)   = error "load failed"
fromLoadSuc (LoadSuccess _ v) = v

{--
(test :: FilePathO2) <- sense "."
prRep = print . senseReport
runEffect $ runSafeP $ for (every (senseRecursive5 test)) (lift . (lift . prRep))
--}

printFileRepresentations :: [(FilePath -> IO String)] -> FilePathO2 -> IO ()
printFileRepresentations cnv (FilePathO2 path FileDataObject _ _ _ _)  = do results <- sequence [(c (b2s path)) | c <- cnv]
                                                                            mapM_ putStrLn results
printFileRepresentations _ _ = do return ()

main = do
        plugins <- loadMagicPlugins
        args <- getArgs
        let path = head args
        let comparators = map (getFileRepresentation) plugins
        (firstElem :: FilePathO2) <- sense path
        runEffect $ runSafeP $
            for (every (senseRecursive5 firstElem)) (lift . (lift . printFileRepresentations comparators))
