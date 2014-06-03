-----------------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module with a class for strings, sucht that the normal ops are
--  all polymorphic
--
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
--    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Strings (BSUTF.ByteString
    , PolyString (..)  -- is this required?
    , s2b, b2s, b2t, t2s, t2b, s2t)   where

import Control.Monad
import GHC.Exts( IsString(..) )
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BSUTF (ByteString, toString, fromString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype MyString = MyString String deriving (Eq, Show)

class PolyString s where
    pshow :: s -> String
    punwords :: [s] -> s
    pputIOwords :: [s] -> IO ()


instance PolyString String where
    pshow = id
    punwords = unwords
    pputIOwords = mapM_ print 

instance PolyString T.Text where
    pshow = show . T.unpack
    punwords = T.unwords
    pputIOwords = mapM_ print . map t2s

instance PolyString BSUTF.ByteString where
    pshow =   show . BSUTF.toString
--instance (IsString m) => PolyString m where
--    pshow = show  . fromString

s2b :: String -> BSUTF.ByteString
s2b = BSUTF.fromString

b2s :: BSUTF.ByteString -> String
b2s = BSUTF.toString

s2t :: String -> T.Text
s2t = T.pack

t2s :: T.Text -> String
t2s = T.unpack

t2b :: T.Text -> BSUTF.ByteString
t2b = encodeUtf8

b2t :: BSUTF.ByteString -> T.Text
b2t = decodeUtf8

