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

outImgLoadResult :: V.VipsOp l ImgLoadResult -> V.VipsOp l ImgLoadResult
outImgLoadResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  flags' <- V.getProperty "flags" opResult' :: (VipsIO GV.ForeignFlags)
  return $ ImgLoadResult { out = out', flags = flags' }


-- Note:
-- output "out" of type Image is aliased as "outImg"
--
outOut :: (Result a ~ Double, HasOutput a "out" Double) => a -> a
outOut = get (Get :: Argument "out")

--
-- The following code has been automatically generated using hvips-gen,
-- from libvips 8.10.6-Tue Jun  1 16:02:28 UTC 2021
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
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  dx0' <- V.getProperty "dx0" opResult' :: (VipsIO Int32)
  dy0' <- V.getProperty "dy0" opResult' :: (VipsIO Int32)
  scale1' <- V.getProperty "scale1" opResult' :: (VipsIO Double)
  angle1' <- V.getProperty "angle1" opResult' :: (VipsIO Double)
  dx1' <- V.getProperty "dx1" opResult' :: (VipsIO Double)
  dy1' <- V.getProperty "dy1" opResult' :: (VipsIO Double)
  return $ MosaicResult { out = out', dx0 = dx0', dy0 = dy0', scale1 = scale1', angle1 = angle1', dx1 = dx1', dy1 = dy1' }

outDrawFloodResult :: V.VipsOp l DrawFloodResult -> V.VipsOp l DrawFloodResult
outDrawFloodResult = V.setOutput $ \opResult' -> do
  left' <- V.getProperty "left" opResult' :: (VipsIO Int32)
  top' <- V.getProperty "top" opResult' :: (VipsIO Int32)
  width' <- V.getProperty "width" opResult' :: (VipsIO Int32)
  height' <- V.getProperty "height" opResult' :: (VipsIO Int32)
  return $ DrawFloodResult { left = left', top = top', width = width', height = height' }

outFillNearestResult :: V.VipsOp l FillNearestResult -> V.VipsOp l FillNearestResult
outFillNearestResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  distance' <- V.getProperty "distance" opResult' :: (VipsIO GV.Image)
  return $ FillNearestResult { out = out', distance = distance' }

outLabelregionsResult :: V.VipsOp l LabelregionsResult -> V.VipsOp l LabelregionsResult
outLabelregionsResult = V.setOutput $ \opResult' -> do
  mask' <- V.getProperty "mask" opResult' :: (VipsIO GV.Image)
  segments' <- V.getProperty "segments" opResult' :: (VipsIO Int32)
  return $ LabelregionsResult { mask = mask', segments = segments' }

outTextResult :: V.VipsOp l TextResult -> V.VipsOp l TextResult
outTextResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  autofit_dpi' <- V.getProperty "autofit_dpi" opResult' :: (VipsIO Int32)
  return $ TextResult { out = out', autofit_dpi = autofit_dpi' }

outAutorotResult :: V.VipsOp l AutorotResult -> V.VipsOp l AutorotResult
outAutorotResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  angle' <- V.getProperty "angle" opResult' :: (VipsIO GV.Angle)
  flip' <- V.getProperty "flip" opResult' :: (VipsIO Bool)
  return $ AutorotResult { out = out', angle = angle', flip = flip' }

outFindTrimResult :: V.VipsOp l FindTrimResult -> V.VipsOp l FindTrimResult
outFindTrimResult = V.setOutput $ \opResult' -> do
  left' <- V.getProperty "left" opResult' :: (VipsIO Int32)
  top' <- V.getProperty "top" opResult' :: (VipsIO Int32)
  width' <- V.getProperty "width" opResult' :: (VipsIO Int32)
  height' <- V.getProperty "height" opResult' :: (VipsIO Int32)
  return $ FindTrimResult { left = left', top = top', width = width', height = height' }

outProfileResult :: V.VipsOp l ProfileResult -> V.VipsOp l ProfileResult
outProfileResult = V.setOutput $ \opResult' -> do
  columns' <- V.getProperty "columns" opResult' :: (VipsIO GV.Image)
  rows' <- V.getProperty "rows" opResult' :: (VipsIO GV.Image)
  return $ ProfileResult { columns = columns', rows = rows' }

outProjectResult :: V.VipsOp l ProjectResult -> V.VipsOp l ProjectResult
outProjectResult = V.setOutput $ \opResult' -> do
  columns' <- V.getProperty "columns" opResult' :: (VipsIO GV.Image)
  rows' <- V.getProperty "rows" opResult' :: (VipsIO GV.Image)
  return $ ProjectResult { columns = columns', rows = rows' }

outMaxResult :: V.VipsOp l MaxResult -> V.VipsOp l MaxResult
outMaxResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO Double)
  x' <- V.getProperty "x" opResult' :: (VipsIO Int32)
  y' <- V.getProperty "y" opResult' :: (VipsIO Int32)
  out_array' <- V.getProperty "out_array" opResult' :: (VipsIO GV.ArrayDouble)
  x_array' <- V.getProperty "x_array" opResult' :: (VipsIO GV.ArrayInt)
  y_array' <- V.getProperty "y_array" opResult' :: (VipsIO GV.ArrayInt)
  return $ MaxResult { out = out', x = x', y = y', out_array = out_array', x_array = x_array', y_array = y_array' }

outMinResult :: V.VipsOp l MinResult -> V.VipsOp l MinResult
outMinResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO Double)
  x' <- V.getProperty "x" opResult' :: (VipsIO Int32)
  y' <- V.getProperty "y" opResult' :: (VipsIO Int32)
  out_array' <- V.getProperty "out_array" opResult' :: (VipsIO GV.ArrayDouble)
  x_array' <- V.getProperty "x_array" opResult' :: (VipsIO GV.ArrayInt)
  y_array' <- V.getProperty "y_array" opResult' :: (VipsIO GV.ArrayInt)
  return $ MinResult { out = out', x = x', y = y', out_array = out_array', x_array = x_array', y_array = y_array' }

outSystemResult :: V.VipsOp l SystemResult -> V.VipsOp l SystemResult
outSystemResult = V.setOutput $ \opResult' -> do
  out' <- V.getProperty "out" opResult' :: (VipsIO GV.Image)
  log' <- V.getProperty "log" opResult' :: (VipsIO T.Text)
  return $ SystemResult { out = out', log = log' }
