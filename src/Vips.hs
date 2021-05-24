
-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips
  ( vips
  , runVips
  , module Vips.VipsIO
  , module Vips.Operations
  ) where

import Vips.VipsIO
import Vips.Operations

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Vips.Internal.VipsOp as V


-- |Execute a chain of bound vips operations
runVips :: MaybeT m a -> m (Maybe a)
runVips = runMaybeT

-- |Combinator to execute a VipsOp within a chain of bound operations.
-- Example usage:
--   runVips $ vips loadImage inFile >>= vips invert
vips :: (a -> V.VipsOp l b) -> (a -> MaybeT VipsIO b)
vips f = MaybeT . V.runVips . f
