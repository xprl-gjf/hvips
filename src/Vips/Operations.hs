{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  LGPL-2.1

module Vips.Operations
  ( loadImage
  , saveImage
  , invert
  ) where

import qualified Data.Text as T

import Vips.VipsCall
import Vips.VipsIO
import Vips.VipsResult
import qualified GI.Vips as V

--
-- Vips image operations:
--
loadImage :: FilePath -> VipsIO (Maybe V.Image)
loadImage f = do
  result <- callVips args =<< V.foreignFindLoad f'
    <|> error ("Failed to load image \"" <> f <> "\": unsupported image format.")
  imgOut <- V.imageNew
  let out = VArg "out" $ VImage imgOut
  result' <- getOutput out result
  clearArg out
  return result'
  where
    f' = T.pack f
    args = [VArg "filename" $ VText f']


invert :: Maybe V.Image -> VipsIO (Maybe V.Image)
invert Nothing  = return Nothing
invert (Just i) = do
  result <- callVips args "invert"
  let out = VArg "out" $ VImage i
  getOutput out result
  where
    args = [VArg "in" $ VImage i]


saveImage :: FilePath -> Maybe V.Image -> VipsIO ()
saveImage _ Nothing = return ()
saveImage f (Just i) = do
  result <- callVips args =<< V.foreignFindSave f'
    <|> error ("Unable to save image \"" <> f <> "\": unsupported image format.")
  getNone result
  where
    f' = T.pack f
    args = [ VArg "in" $ VImage i
           , VArg "filename" $ VText f'
           ]
