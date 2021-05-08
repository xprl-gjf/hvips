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
import           Prelude hiding (log)

import qualified GI.Vips as GV
import qualified Vips.Internal.VipsOp as V
import           Vips.Introspection.Operations
import           Vips.VipsIO

--
-- Vips named result properties:
--

-- Note:
-- output "out" of type Image is aliased as "outImg"
--
outOut :: (Result a ~ Double, HasOutput a "out" Double) => a -> a
outOut = get (Get :: Argument "out")
