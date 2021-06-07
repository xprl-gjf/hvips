
-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3
--
-- Bindings to the libvips image processing library.
--
-- > import           Vips
-- > import qualified Vips.Arguments as VArg
-- >
-- > main = withVips init $ do
-- >   vips . loadImage >=> vips . blur >=> vips . saveImage outFile $ inFile
-- >   where
-- >     inFile = "image.jpg"
-- >     outFile = "result.png"
-- >     blur = gaussblur 1.2 <&> VArg.minAmpl (0.025 :: Double)
-- >     init = VipsInit { progName = "hvips-demo", checkLeaks = Default }

module Vips
  ( vips
  , module Vips.VipsIO
  , module Vips.Operations
  , module Vips.Introspection.Operations
  , module Vips.Results
  ) where

import Vips.VipsIO
import Vips.Operations
import Vips.Results (freeBlobBuffer, freeArrayDouble, freeArrayInt)
import Vips.Introspection.Operations
  ( ImgLoadResult(..)
  , MosaicResult(..)
  , DrawFloodResult(..)
  , FillNearestResult(..)
  , LabelregionsResult(..)
  , TextResult(..)
  , AutorotResult(..)
  , FindTrimResult(..)
  , ProfileResult(..)
  , ProjectResult(..)
  , MaxResult(..)
  , MinResult(..)
  , SystemResult(..)
  )

import qualified Vips.Internal.VipsOp as V

-- | Convenient shorthand to run a vips operation
vips :: V.VipsOp l a -> VipsIO a
vips = V.runVips
