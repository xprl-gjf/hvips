
-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips
  ( vips
  , module Vips.VipsIO
  , module Vips.Operations
  ) where

import Vips.VipsIO
import Vips.Operations
import qualified Vips.Internal.VipsOp as V

-- | Convenient shorthand to run a vips operation
vips :: V.VipsOp l a -> VipsIO a
vips = V.runVips
