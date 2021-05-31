{-# LANGUAGE OverloadedStrings,
             DisambiguateRecordFields,
             DataKinds,
             TypeFamilies,
             FlexibleContexts #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Results where

import           Data.Int
import qualified Data.Text as T
import qualified Data.GI.Base as GI
import qualified Data.GI.Base.ShortPrelude as SP (liftIO)
import           Prelude hiding (log)

import qualified GI.Vips as GV
import qualified Vips.Internal.VipsOp as V
import           Vips.Introspection.Operations
import           Vips.VipsIO

-- |Free the memory for a GV.Blob returned from a vips operation via outBuffer.
freeBlobBuffer :: GV.Blob -> VipsIO ()
freeBlobBuffer = SP.liftIO . GI.freeBoxed

-- |Free the memory for a GV.ArrayDouble returned from a vips operation via outOutArray.
freeArrayDouble :: GV.ArrayDouble -> VipsIO ()
freeArrayDouble = SP.liftIO . GI.freeBoxed

-- |Free the memory for a GV.ArrayInt returned from a vips operation via e.g. outXArray.
freeArrayInt :: GV.ArrayInt -> VipsIO ()
freeArrayInt = SP.liftIO . GI.freeBoxed

--
-- Vips named result properties:
--

-- Note:
-- output "out" of type Image is aliased as "outImg"
--
outOut :: (Result a ~ Double, HasOutput a "out" Double) => a -> a
outOut = get (Get :: Argument "out")
