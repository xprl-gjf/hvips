{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, AllowAmbiguousTypes #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Operations
  ( module Vips.Operations
  , SystemResult(..)
  ) where

import           Data.Int
import           Data.Word
import           Data.Function ((&))
import qualified Data.Text as T

import qualified GI.Vips as GV
import           Vips.Internal.VipsOp
import qualified Vips.Internal.VipsOp as V (void)
import           Vips.Introspection.Operations
import qualified Vips.Arguments as V
import qualified Vips.Results as V


-- |Convenient reverse function application for applying attributes
-- |to partially constructed Vips operations.
-- Example usage:
--   gaussBlur 1.2 <&> minAmpl (1.8 :: Double)
(<&>) :: (a -> b) -> (b -> b) -> (a -> b)
infixl 4 <&>
f <&> g = g . f

--
-- Vips foreign operations:
--
loadImage :: FilePath -> LoadImage
loadImage a = vipsForeignOp loader (Foreign :: Nickname "foreignLoadImage") & inputs & outputs
  where
    a' = T.pack a
    loader = GV.foreignFindLoad a'
    inputs = V.filename' a'
    outputs = V.outImg

saveImage :: FilePath -> GV.Image -> SaveImage
saveImage a b = vipsForeignOp saver (Foreign :: Nickname "foreignSaveImage") & inputs & outputs
  where
    a' = T.pack a
    saver = GV.foreignFindSave a'
    inputs = V.filename' a' . V.img b
    outputs = V.void

--
-- Vips image operations:
--
