-----------------------------------------------------------------------------
--
-- Module      :  Sensing
-- Copyright   :
--
-- | the data for the object which is sensed
-- operation to sense the object
-- possibly with a recursive version
-- and to output the sensed data
--
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
--    , FunctionalDependencies
--    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Sensing (Sensing (..),
                FilePathO2 (FilePathO2),
                FilePathX,
                infoPath,
                FPtype (FileDataObject)
               ) where

import Strings (t2s, s2t, b2s, s2b, ByteString)
import qualified Data.ByteString as BS (null)

import qualified System.Posix.ByteString as Posix
import System.Posix.FilePath ((</>))
import System.Directory (canonicalizePath)

import Pipes (yield, ListT(..), ListT, liftIO)
import Pipes.Safe (Base, MonadSafe, bracket)

import qualified Control.Exception as E (SomeException, catch)

import Control.Monad (unless, when)
import Control.Applicative (Alternative(..), Applicative(..))
import Data.Text (Text)

type FilePathX = ByteString

debug_fp = False

data FPtype = FileDataObject | FPdir | FPspecial | FPunreadable  -- | FPzerosize
            | FPlink | FPbrokenLink | FNotFile
    deriving (Show, Read, Eq)
    -- these are all the subtypes of FPathO

-- | descibes an entry in a directory - file or directory!
data FilePathO2 = FilePathO2 { infoPath ::  FilePathX
           , infoType :: FPtype
           , infoStatus ::  Posix.FileStatus
           , infoIsReadable :: Bool
           , infoLink :: FilePathX
           , infoMD5 :: Text
--           , canonicalPath :: FilePath -- CanonicalFilePath
           }
                      -- deriving (Eq, Ord, Show)


-- | the operations for sensing an object - e.g. a filePath
class Sensing fpo where
    sense :: FilePath -> IO fpo
    -- ^ does a get stat and keeps the data

    senseRecursive5 :: (MonadSafe m, Base m ~ IO) => fpo -> ListT m fpo
    senseReport :: fpo -> String

instance Sensing FilePathO2 where
    sense fn = do
        let fnb = s2b fn
        let fp = FilePathO2 {infoPath = fnb
                , infoType = undef "infoType initial"
                , infoStatus = undef "infoStatus initial"
                , infoIsReadable = undef "infoisreadable initial"
                , infoLink = undef "infoLink initial"
                , infoMD5 = undef "infoMD5 initial" }
--        c <- canonicalizePath  $ fn
        st <- Posix.getSymbolicLinkStatus fnb
        readable <-  Posix.fileAccess fnb True False False
        let isRegular = Posix.isRegularFile st
        let isDir = Posix.isDirectory st
        let isLink = Posix.isSymbolicLink st
        let fileType = if not readable then FPunreadable
                        else if isLink then FPlink
                            else if isDir then FPdir
                                else if isRegular then FileDataObject
                                    else FPspecial
        let fp2 = fp {infoType = fileType
                , infoStatus = st
                , infoIsReadable = readable}
        fp3 <-
            case fileType of
                FPlink -> do
                    lk <- Posix.readSymbolicLink fnb
--                    let lk = "not valid"
                    return (fp2 {infoLink = lk})
                _ -> return fp2

        return  fp3

        `E.catch`
            \(e::E.SomeException) ->  case e of
                _ ->  do
--                    when debug_fp $
                    putStrLn . unwords $ ["sense exception - assume broken link", show e]
--                    lk <- Posix.readSymbolicLink . s2b $ fn
                    return $ FilePathO2 {infoPath = s2b fn
                            , infoType = FPbrokenLink
--                            , infoLink = "broken" -- lk
                            , infoStatus = undef "sense exception status"
                            , infoIsReadable = undef "sense exception isReadable"
                            , infoMD5 = undef "infoMD5 exception" }
                        -- could be nofile?

    senseRecursive5 fpo =  do
        when debug_fp $ liftIO . putStrLn . unwords $ ["senseRecursive4 start"
                , b2s . infoPath $ fpo, show . infoType $ fpo]
        case (infoType fpo) of

            FPbrokenLink -> do
                        when debug_fp $ liftIO . putStrLn . unwords $
                            ["senseRecursive4 brokenlink", b2s . infoPath $ fpo]
                        pure fpo
            FPlink -> pure fpo
            FPunreadable -> pure fpo
            FPspecial -> pure fpo
            FileDataObject -> pure fpo -- (   (pure fdo3) <|> (pure fdt3))
            FPdir -> pure fpo  <|>     senseRecursiveFolder5   fpo
            _ -> do
                    liftIO . putStrLn . unwords $
                            ["senseRecursive4 ignored"
                                , b2s . infoPath $ fpo, show . infoType $ fpo]
                    pure fpo


    senseReport fpo = unwords [path, ftype, "\n\t", md]
        where
            path = b2s . infoPath $ fpo
            ftype = show . infoType $ fpo
            md = case infoType fpo of
                FileDataObject -> "File \t" ++ path
                FPlink -> "Link \t" ++ (b2s . infoLink $ fpo)
                _ -> ""


senseRecursiveFolder5 ::  (MonadSafe m, Base m ~ IO) => FilePathO2 -> ListT m FilePathO2
senseRecursiveFolder5 fpo = do
        when debug_fp $ liftIO . putStrLn . unwords $ ["senseRecursiveFolder5"
                            , b2s . infoPath $ fpo, show . infoType $ fpo]
        (fpo'::FilePathO2) <- readDirStream5  fpo
        senseRecursive5 fpo'

readDirStream5  fpo = Select $ do
        bracket (Posix.openDirStream .  infoPath $ fpo) Posix.closeDirStream $ \dirp -> do
            let loop = do
                file' <- liftIO $  Posix.readDirStream dirp
                unless (BS.null file') $ do
                    when (file' /= "." && file' /= "..") $ do
                        let d = infoPath fpo
                        let d' =  d </>   file'
                        when debug_fp $ liftIO . putStrLn . unwords $ ["readDir", show d']

                        fpo2 <- liftIO . sense . b2s $ d'

                        when debug_fp $ liftIO . putStrLn . unwords $ ["readDir fpo2"
                                , b2s . infoPath $ fpo2, show . infoType $ fpo2]
                        yield fpo2
                    loop
            loop


undef s = error s
