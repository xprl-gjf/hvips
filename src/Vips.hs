
-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

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
  ( MosaicResult(..)
  , DrawFloodResult(..)
  , FillNearestResult(..)
  , LabelregionsResult(..)
  , HeifloadSourceResult(..)
  , HeifloadBufferResult(..)
  , HeifloadResult(..)
  , OpenexrloadResult(..)
  , FitsloadResult(..)
  , MagickloadBufferResult(..)
  , MagickloadResult(..)
  , TiffloadSourceResult(..)
  , TiffloadBufferResult(..)
  , TiffloadResult(..)
  , WebploadSourceResult(..)
  , WebploadBufferResult(..)
  , WebploadResult(..)
  , JpegloadSourceResult(..)
  , JpegloadBufferResult(..)
  , JpegloadResult(..)
  , PngloadSourceResult(..)
  , PngloadBufferResult(..)
  , PngloadResult(..)
  , GifloadSourceResult(..)
  , GifloadBufferResult(..)
  , GifloadResult(..)
  , SvgloadSourceResult(..)
  , SvgloadBufferResult(..)
  , SvgloadResult(..)
  , PdfloadSourceResult(..)
  , PdfloadBufferResult(..)
  , PdfloadResult(..)
  , RadloadSourceResult(..)
  , RadloadBufferResult(..)
  , RadloadResult(..)
  , PpmloadSourceResult(..)
  , PpmloadResult(..)
  , AnalyzeloadResult(..)
  , VipsloadResult(..)
  , RawloadResult(..)
  , MatrixloadSourceResult(..)
  , MatrixloadResult(..)
  , CsvloadSourceResult(..)
  , CsvloadResult(..)
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
