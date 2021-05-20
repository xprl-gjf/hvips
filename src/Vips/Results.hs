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

--
-- The following code has been automatically generated using hvips-gen,
-- from libvips 8.10.6-Sat Apr 24 19:02:40 UTC 2021
--


outAngle :: (Result a ~ GV.Angle, HasOutput a "angle" GV.Angle) => a -> a
outAngle = get (Get :: Argument "angle")

outAngle1 :: (Result a ~ Double, HasOutput a "angle1" Double) => a -> a
outAngle1 = get (Get :: Argument "angle1")

outAutofitDpi :: (Result a ~ Int32, HasOutput a "autofit-dpi" Int32) => a -> a
outAutofitDpi = get (Get :: Argument "autofit-dpi")

outBuffer :: (Result a ~ GV.Blob, HasOutput a "buffer" GV.Blob) => a -> a
outBuffer = get (Get :: Argument "buffer")

outColumns :: (Result a ~ GV.Image, HasOutput a "columns" GV.Image) => a -> a
outColumns = get (Get :: Argument "columns")

outDistance :: (Result a ~ GV.Image, HasOutput a "distance" GV.Image) => a -> a
outDistance = get (Get :: Argument "distance")

outDx0 :: (Result a ~ Int32, HasOutput a "dx0" Int32) => a -> a
outDx0 = get (Get :: Argument "dx0")

outDx1 :: (Result a ~ Double, HasOutput a "dx1" Double) => a -> a
outDx1 = get (Get :: Argument "dx1")

outDy0 :: (Result a ~ Int32, HasOutput a "dy0" Int32) => a -> a
outDy0 = get (Get :: Argument "dy0")

outDy1 :: (Result a ~ Double, HasOutput a "dy1" Double) => a -> a
outDy1 = get (Get :: Argument "dy1")

outFlags :: (Result a ~ GV.ForeignFlags, HasOutput a "flags" GV.ForeignFlags) => a -> a
outFlags = get (Get :: Argument "flags")

outFlip :: (Result a ~ Bool, HasOutput a "flip" Bool) => a -> a
outFlip = get (Get :: Argument "flip")

outHeight :: (Result a ~ Int32, HasOutput a "height" Int32) => a -> a
outHeight = get (Get :: Argument "height")

outLeft :: (Result a ~ Int32, HasOutput a "left" Int32) => a -> a
outLeft = get (Get :: Argument "left")

outLog :: (Result a ~ T.Text, HasOutput a "log" T.Text) => a -> a
outLog = get (Get :: Argument "log")

outMask :: (Result a ~ GV.Image, HasOutput a "mask" GV.Image) => a -> a
outMask = get (Get :: Argument "mask")

outMonotonic :: (Result a ~ Bool, HasOutput a "monotonic" Bool) => a -> a
outMonotonic = get (Get :: Argument "monotonic")

outNolines :: (Result a ~ Double, HasOutput a "nolines" Double) => a -> a
outNolines = get (Get :: Argument "nolines")

outImg :: (Result a ~ GV.Image, HasOutput a "out" GV.Image) => a -> a
outImg = get (Get :: Argument "out")

outOutArray :: (Result a ~ GV.ArrayDouble, HasOutput a "out-array" GV.ArrayDouble) => a -> a
outOutArray = get (Get :: Argument "out-array")

outProfile :: (Result a ~ GV.Blob, HasOutput a "profile" GV.Blob) => a -> a
outProfile = get (Get :: Argument "profile")

outRows :: (Result a ~ GV.Image, HasOutput a "rows" GV.Image) => a -> a
outRows = get (Get :: Argument "rows")

outScale1 :: (Result a ~ Double, HasOutput a "scale1" Double) => a -> a
outScale1 = get (Get :: Argument "scale1")

outSegments :: (Result a ~ Int32, HasOutput a "segments" Int32) => a -> a
outSegments = get (Get :: Argument "segments")

outThreshold :: (Result a ~ Int32, HasOutput a "threshold" Int32) => a -> a
outThreshold = get (Get :: Argument "threshold")

outTop :: (Result a ~ Int32, HasOutput a "top" Int32) => a -> a
outTop = get (Get :: Argument "top")

outWidth :: (Result a ~ Int32, HasOutput a "width" Int32) => a -> a
outWidth = get (Get :: Argument "width")

outX :: (Result a ~ Int32, HasOutput a "x" Int32) => a -> a
outX = get (Get :: Argument "x")

outXArray :: (Result a ~ GV.ArrayInt, HasOutput a "x-array" GV.ArrayInt) => a -> a
outXArray = get (Get :: Argument "x-array")

outY :: (Result a ~ Int32, HasOutput a "y" Int32) => a -> a
outY = get (Get :: Argument "y")

outYArray :: (Result a ~ GV.ArrayInt, HasOutput a "y-array" GV.ArrayInt) => a -> a
outYArray = get (Get :: Argument "y-array")

outMosaicResult :: V.VipsOp l MosaicResult -> V.VipsOp l MosaicResult
outMosaicResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  dx0' <- V.getProperty "dx0" opResult' :: (VipsIO (Maybe Int32))
  dy0' <- V.getProperty "dy0" opResult' :: (VipsIO (Maybe Int32))
  scale1' <- V.getProperty "scale1" opResult' :: (VipsIO (Maybe Double))
  angle1' <- V.getProperty "angle1" opResult' :: (VipsIO (Maybe Double))
  dx1' <- V.getProperty "dx1" opResult' :: (VipsIO (Maybe Double))
  dy1' <- V.getProperty "dy1" opResult' :: (VipsIO (Maybe Double))
  return $ Just MosaicResult { out = out', dx0 = dx0', dy0 = dy0', scale1 = scale1', angle1 = angle1', dx1 = dx1', dy1 = dy1' }

outDrawFloodResult :: V.VipsOp l DrawFloodResult -> V.VipsOp l DrawFloodResult
outDrawFloodResult = V.setOutput $ \opResult' -> do
  left' <- V.getProperty "left" opResult' :: (VipsIO (Maybe Int32))
  top' <- V.getProperty "top" opResult' :: (VipsIO (Maybe Int32))
  width' <- V.getProperty "width" opResult' :: (VipsIO (Maybe Int32))
  height' <- V.getProperty "height" opResult' :: (VipsIO (Maybe Int32))
  return $ Just DrawFloodResult { left = left', top = top', width = width', height = height' }

outFillNearestResult :: V.VipsOp l FillNearestResult -> V.VipsOp l FillNearestResult
outFillNearestResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  distance' <- V.getProperty "distance" opResult' :: (VipsIO (Maybe GV.Image))
  return $ Just FillNearestResult { out = out', distance = distance' }

outLabelregionsResult :: V.VipsOp l LabelregionsResult -> V.VipsOp l LabelregionsResult
outLabelregionsResult = V.setOutput $ \opResult' -> do
  mask' <- V.getProperty "mask" opResult' :: (VipsIO (Maybe GV.Image))
  segments' <- V.getProperty "segments" opResult' :: (VipsIO (Maybe Int32))
  return $ Just LabelregionsResult { mask = mask', segments = segments' }

outHeifloadSourceResult :: V.VipsOp l HeifloadSourceResult -> V.VipsOp l HeifloadSourceResult
outHeifloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just HeifloadSourceResult { out = out', flags = flags' }

outHeifloadBufferResult :: V.VipsOp l HeifloadBufferResult -> V.VipsOp l HeifloadBufferResult
outHeifloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just HeifloadBufferResult { out = out', flags = flags' }

outHeifloadResult :: V.VipsOp l HeifloadResult -> V.VipsOp l HeifloadResult
outHeifloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just HeifloadResult { out = out', flags = flags' }

outOpenexrloadResult :: V.VipsOp l OpenexrloadResult -> V.VipsOp l OpenexrloadResult
outOpenexrloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just OpenexrloadResult { out = out', flags = flags' }

outFitsloadResult :: V.VipsOp l FitsloadResult -> V.VipsOp l FitsloadResult
outFitsloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just FitsloadResult { out = out', flags = flags' }

outMagickloadBufferResult :: V.VipsOp l MagickloadBufferResult -> V.VipsOp l MagickloadBufferResult
outMagickloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just MagickloadBufferResult { out = out', flags = flags' }

outMagickloadResult :: V.VipsOp l MagickloadResult -> V.VipsOp l MagickloadResult
outMagickloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just MagickloadResult { out = out', flags = flags' }

outTiffloadSourceResult :: V.VipsOp l TiffloadSourceResult -> V.VipsOp l TiffloadSourceResult
outTiffloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just TiffloadSourceResult { out = out', flags = flags' }

outTiffloadBufferResult :: V.VipsOp l TiffloadBufferResult -> V.VipsOp l TiffloadBufferResult
outTiffloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just TiffloadBufferResult { out = out', flags = flags' }

outTiffloadResult :: V.VipsOp l TiffloadResult -> V.VipsOp l TiffloadResult
outTiffloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just TiffloadResult { out = out', flags = flags' }

outWebploadSourceResult :: V.VipsOp l WebploadSourceResult -> V.VipsOp l WebploadSourceResult
outWebploadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just WebploadSourceResult { out = out', flags = flags' }

outWebploadBufferResult :: V.VipsOp l WebploadBufferResult -> V.VipsOp l WebploadBufferResult
outWebploadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just WebploadBufferResult { out = out', flags = flags' }

outWebploadResult :: V.VipsOp l WebploadResult -> V.VipsOp l WebploadResult
outWebploadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just WebploadResult { out = out', flags = flags' }

outJpegloadSourceResult :: V.VipsOp l JpegloadSourceResult -> V.VipsOp l JpegloadSourceResult
outJpegloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just JpegloadSourceResult { out = out', flags = flags' }

outJpegloadBufferResult :: V.VipsOp l JpegloadBufferResult -> V.VipsOp l JpegloadBufferResult
outJpegloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just JpegloadBufferResult { out = out', flags = flags' }

outJpegloadResult :: V.VipsOp l JpegloadResult -> V.VipsOp l JpegloadResult
outJpegloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just JpegloadResult { out = out', flags = flags' }

outPngloadSourceResult :: V.VipsOp l PngloadSourceResult -> V.VipsOp l PngloadSourceResult
outPngloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PngloadSourceResult { out = out', flags = flags' }

outPngloadBufferResult :: V.VipsOp l PngloadBufferResult -> V.VipsOp l PngloadBufferResult
outPngloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PngloadBufferResult { out = out', flags = flags' }

outPngloadResult :: V.VipsOp l PngloadResult -> V.VipsOp l PngloadResult
outPngloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PngloadResult { out = out', flags = flags' }

outGifloadSourceResult :: V.VipsOp l GifloadSourceResult -> V.VipsOp l GifloadSourceResult
outGifloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just GifloadSourceResult { out = out', flags = flags' }

outGifloadBufferResult :: V.VipsOp l GifloadBufferResult -> V.VipsOp l GifloadBufferResult
outGifloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just GifloadBufferResult { out = out', flags = flags' }

outGifloadResult :: V.VipsOp l GifloadResult -> V.VipsOp l GifloadResult
outGifloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just GifloadResult { out = out', flags = flags' }

outSvgloadSourceResult :: V.VipsOp l SvgloadSourceResult -> V.VipsOp l SvgloadSourceResult
outSvgloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just SvgloadSourceResult { out = out', flags = flags' }

outSvgloadBufferResult :: V.VipsOp l SvgloadBufferResult -> V.VipsOp l SvgloadBufferResult
outSvgloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just SvgloadBufferResult { out = out', flags = flags' }

outSvgloadResult :: V.VipsOp l SvgloadResult -> V.VipsOp l SvgloadResult
outSvgloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just SvgloadResult { out = out', flags = flags' }

outPdfloadSourceResult :: V.VipsOp l PdfloadSourceResult -> V.VipsOp l PdfloadSourceResult
outPdfloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PdfloadSourceResult { out = out', flags = flags' }

outPdfloadBufferResult :: V.VipsOp l PdfloadBufferResult -> V.VipsOp l PdfloadBufferResult
outPdfloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PdfloadBufferResult { out = out', flags = flags' }

outPdfloadResult :: V.VipsOp l PdfloadResult -> V.VipsOp l PdfloadResult
outPdfloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PdfloadResult { out = out', flags = flags' }

outRadloadSourceResult :: V.VipsOp l RadloadSourceResult -> V.VipsOp l RadloadSourceResult
outRadloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just RadloadSourceResult { out = out', flags = flags' }

outRadloadBufferResult :: V.VipsOp l RadloadBufferResult -> V.VipsOp l RadloadBufferResult
outRadloadBufferResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just RadloadBufferResult { out = out', flags = flags' }

outRadloadResult :: V.VipsOp l RadloadResult -> V.VipsOp l RadloadResult
outRadloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just RadloadResult { out = out', flags = flags' }

outPpmloadSourceResult :: V.VipsOp l PpmloadSourceResult -> V.VipsOp l PpmloadSourceResult
outPpmloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PpmloadSourceResult { out = out', flags = flags' }

outPpmloadResult :: V.VipsOp l PpmloadResult -> V.VipsOp l PpmloadResult
outPpmloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just PpmloadResult { out = out', flags = flags' }

outAnalyzeloadResult :: V.VipsOp l AnalyzeloadResult -> V.VipsOp l AnalyzeloadResult
outAnalyzeloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just AnalyzeloadResult { out = out', flags = flags' }

outVipsloadResult :: V.VipsOp l VipsloadResult -> V.VipsOp l VipsloadResult
outVipsloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just VipsloadResult { out = out', flags = flags' }

outRawloadResult :: V.VipsOp l RawloadResult -> V.VipsOp l RawloadResult
outRawloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just RawloadResult { out = out', flags = flags' }

outMatrixloadSourceResult :: V.VipsOp l MatrixloadSourceResult -> V.VipsOp l MatrixloadSourceResult
outMatrixloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just MatrixloadSourceResult { out = out', flags = flags' }

outMatrixloadResult :: V.VipsOp l MatrixloadResult -> V.VipsOp l MatrixloadResult
outMatrixloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just MatrixloadResult { out = out', flags = flags' }

outCsvloadSourceResult :: V.VipsOp l CsvloadSourceResult -> V.VipsOp l CsvloadSourceResult
outCsvloadSourceResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just CsvloadSourceResult { out = out', flags = flags' }

outCsvloadResult :: V.VipsOp l CsvloadResult -> V.VipsOp l CsvloadResult
outCsvloadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  flags' <- V.getProperty "flags" opResult' :: (VipsIO (Maybe GV.ForeignFlags))
  return $ Just CsvloadResult { out = out', flags = flags' }

outTextResult :: V.VipsOp l TextResult -> V.VipsOp l TextResult
outTextResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  autofit_dpi' <- V.getProperty "autofit_dpi" opResult' :: (VipsIO (Maybe Int32))
  return $ Just TextResult { out = out', autofit_dpi = autofit_dpi' }

outAutorotResult :: V.VipsOp l AutorotResult -> V.VipsOp l AutorotResult
outAutorotResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  angle' <- V.getProperty "angle" opResult' :: (VipsIO (Maybe GV.Angle))
  flip' <- V.getProperty "flip" opResult' :: (VipsIO (Maybe Bool))
  return $ Just AutorotResult { out = out', angle = angle', flip = flip' }

outFindTrimResult :: V.VipsOp l FindTrimResult -> V.VipsOp l FindTrimResult
outFindTrimResult = V.setOutput $ \opResult' -> do
  left' <- V.getProperty "left" opResult' :: (VipsIO (Maybe Int32))
  top' <- V.getProperty "top" opResult' :: (VipsIO (Maybe Int32))
  width' <- V.getProperty "width" opResult' :: (VipsIO (Maybe Int32))
  height' <- V.getProperty "height" opResult' :: (VipsIO (Maybe Int32))
  return $ Just FindTrimResult { left = left', top = top', width = width', height = height' }

outProfileResult :: V.VipsOp l ProfileResult -> V.VipsOp l ProfileResult
outProfileResult = V.setOutput $ \opResult' -> do
  columns' <- V.getProperty "columns" opResult' :: (VipsIO (Maybe GV.Image))
  rows' <- V.getProperty "rows" opResult' :: (VipsIO (Maybe GV.Image))
  return $ Just ProfileResult { columns = columns', rows = rows' }

outProjectResult :: V.VipsOp l ProjectResult -> V.VipsOp l ProjectResult
outProjectResult = V.setOutput $ \opResult' -> do
  columns' <- V.getProperty "columns" opResult' :: (VipsIO (Maybe GV.Image))
  rows' <- V.getProperty "rows" opResult' :: (VipsIO (Maybe GV.Image))
  return $ Just ProjectResult { columns = columns', rows = rows' }

outMaxResult :: V.VipsOp l MaxResult -> V.VipsOp l MaxResult
outMaxResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe Double))
  x' <- V.getProperty "x" opResult' :: (VipsIO (Maybe Int32))
  y' <- V.getProperty "y" opResult' :: (VipsIO (Maybe Int32))
  out_array' <- V.getProperty "out_array" opResult' :: (VipsIO (Maybe GV.ArrayDouble))
  x_array' <- V.getProperty "x_array" opResult' :: (VipsIO (Maybe GV.ArrayInt))
  y_array' <- V.getProperty "y_array" opResult' :: (VipsIO (Maybe GV.ArrayInt))
  return $ Just MaxResult { out = out', x = x', y = y', out_array = out_array', x_array = x_array', y_array = y_array' }

outMinResult :: V.VipsOp l MinResult -> V.VipsOp l MinResult
outMinResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe Double))
  x' <- V.getProperty "x" opResult' :: (VipsIO (Maybe Int32))
  y' <- V.getProperty "y" opResult' :: (VipsIO (Maybe Int32))
  out_array' <- V.getProperty "out_array" opResult' :: (VipsIO (Maybe GV.ArrayDouble))
  x_array' <- V.getProperty "x_array" opResult' :: (VipsIO (Maybe GV.ArrayInt))
  y_array' <- V.getProperty "y_array" opResult' :: (VipsIO (Maybe GV.ArrayInt))
  return $ Just MinResult { out = out', x = x', y = y', out_array = out_array', x_array = x_array', y_array = y_array' }

outSystemResult :: V.VipsOp l SystemResult -> V.VipsOp l SystemResult
outSystemResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO (Maybe GV.Image))
  log' <- V.getProperty "log" opResult' :: (VipsIO (Maybe T.Text))
  return $ Just SystemResult { out = out', log = log' }
