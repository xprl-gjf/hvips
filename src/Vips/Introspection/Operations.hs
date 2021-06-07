{-# LANGUAGE DuplicateRecordFields,
             DataKinds, TypeFamilies,
             MultiParamTypeClasses, FlexibleInstances #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Introspection.Operations where

import           Data.Int
import           Data.Kind (Type)
import qualified Data.Text as T
import           Data.Word
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified GI.Vips as GV
import qualified Vips.Internal.VipsOp as V
import           Vips.Internal.VipsOp (VipsOp, VipsResult, getProperty, IsVipsArg, IsVipsOutput)
import           Vips.VipsIO (VipsIO)

--
-- Named argument handling:
--
data Argument (l :: Symbol) = Set | Get

-- FIXME:
data VipsSource
instance IsVipsArg VipsSource
data VipsTarget
instance IsVipsArg VipsTarget


class IsVipsOp a where
  type Result a :: Type
  setInput :: (IsVipsArg b) => T.Text -> b -> a -> a
  setOutput :: (VipsResult -> VipsIO (Result a)) -> a -> a

instance IsVipsOp (VipsOp l a) where
  type Result (VipsOp l a) = a
  setInput = V.setInput
  setOutput = V.setOutput

class (KnownSymbol l, IsVipsOp a, IsVipsArg b)  => HasArgument a l b where
  -- |Apply a named argument to a Vips operation
  set :: Argument l -> b -> a -> a
  set l = setInput (attrName l)

class (KnownSymbol l, IsVipsOp a, IsVipsOutput b) => HasOutput a l b where
  -- |Get a named result from a Vips operation
  get :: (Result a ~ b) => Argument l -> a -> a
  get l = setOutput (getProperty (attrName l))

attrName :: (KnownSymbol l) => Argument l -> T.Text
attrName = T.pack . symbolVal

--
-- Vips foreign operations:
--
type LoadImage = VipsOp "foreignLoadImage" GV.Image
instance HasArgument LoadImage "filename" T.Text
instance HasOutput LoadImage "out" GV.Image

type SaveImage = VipsOp "foreignSaveImage" ()
instance HasArgument SaveImage "in" GV.Image
instance HasArgument SaveImage "filename" T.Text

--
-- Vips image operations:
--

data ImgLoadResult = ImgLoadResult { out :: GV.Image, flags :: GV.ForeignFlags }

--
-- The following code has been automatically generated using hvips-gen,
-- from libvips 8.10.6-Tue Jun  1 16:02:28 UTC 2021
--


type Globalbalance = VipsOp "globalbalance" GV.Image
instance HasArgument Globalbalance "in" GV.Image            -- Input image
instance HasArgument Globalbalance "gamma" Double           -- Image gamma
instance HasArgument Globalbalance "int-output" Bool        -- Integer output
instance HasOutput Globalbalance "out" GV.Image             -- Output image

type Match = VipsOp "match" GV.Image
instance HasArgument Match "ref" GV.Image                   -- Reference image
instance HasArgument Match "sec" GV.Image                   -- Secondary image
instance HasArgument Match "xr1" Int32                      -- Position of first reference tie-point
instance HasArgument Match "yr1" Int32                      -- Position of first reference tie-point
instance HasArgument Match "xs1" Int32                      -- Position of first secondary tie-point
instance HasArgument Match "ys1" Int32                      -- Position of first secondary tie-point
instance HasArgument Match "xr2" Int32                      -- Position of second reference tie-point
instance HasArgument Match "yr2" Int32                      -- Position of second reference tie-point
instance HasArgument Match "xs2" Int32                      -- Position of second secondary tie-point
instance HasArgument Match "ys2" Int32                      -- Position of second secondary tie-point
instance HasArgument Match "hwindow" Int32                  -- Half window size
instance HasArgument Match "harea" Int32                    -- Half area size
instance HasArgument Match "search" Bool                    -- Search to improve tie-points
instance HasArgument Match "interpolate" GV.Interpolate     -- Interpolate pixels with this
instance HasOutput Match "out" GV.Image                     -- Output image

type Matrixinvert = VipsOp "matrixinvert" GV.Image
instance HasArgument Matrixinvert "in" GV.Image             -- An square matrix
instance HasOutput Matrixinvert "out" GV.Image              -- Output matrix

type Mosaic1 = VipsOp "mosaic1" GV.Image
instance HasArgument Mosaic1 "ref" GV.Image                 -- Reference image
instance HasArgument Mosaic1 "sec" GV.Image                 -- Secondary image
instance HasArgument Mosaic1 "direction" GV.Direction       -- Horizontal or vertical mosaic
instance HasArgument Mosaic1 "xr1" Int32                    -- Position of first reference tie-point
instance HasArgument Mosaic1 "yr1" Int32                    -- Position of first reference tie-point
instance HasArgument Mosaic1 "xs1" Int32                    -- Position of first secondary tie-point
instance HasArgument Mosaic1 "ys1" Int32                    -- Position of first secondary tie-point
instance HasArgument Mosaic1 "xr2" Int32                    -- Position of second reference tie-point
instance HasArgument Mosaic1 "yr2" Int32                    -- Position of second reference tie-point
instance HasArgument Mosaic1 "xs2" Int32                    -- Position of second secondary tie-point
instance HasArgument Mosaic1 "ys2" Int32                    -- Position of second secondary tie-point
instance HasArgument Mosaic1 "hwindow" Int32                -- Half window size
instance HasArgument Mosaic1 "harea" Int32                  -- Half area size
instance HasArgument Mosaic1 "search" Bool                  -- Search to improve tie-points
instance HasArgument Mosaic1 "interpolate" GV.Interpolate   -- Interpolate pixels with this
instance HasArgument Mosaic1 "mblend" Int32                 -- Maximum blend size
instance HasArgument Mosaic1 "bandno" Int32                 -- Band to search for features on
instance HasOutput Mosaic1 "out" GV.Image                   -- Output image

type Mosaic = VipsOp "mosaic" MosaicResult
data MosaicResult = MosaicResult { out :: GV.Image, dx0 :: Int32, dy0 :: Int32, scale1 :: Double, angle1 :: Double, dx1 :: Double, dy1 :: Double }
instance HasArgument Mosaic "ref" GV.Image                  -- Reference image
instance HasArgument Mosaic "sec" GV.Image                  -- Secondary image
instance HasArgument Mosaic "direction" GV.Direction        -- Horizontal or vertical mosaic
instance HasArgument Mosaic "xref" Int32                    -- Position of reference tie-point
instance HasArgument Mosaic "yref" Int32                    -- Position of reference tie-point
instance HasArgument Mosaic "xsec" Int32                    -- Position of secondary tie-point
instance HasArgument Mosaic "ysec" Int32                    -- Position of secondary tie-point
instance HasArgument Mosaic "hwindow" Int32                 -- Half window size
instance HasArgument Mosaic "harea" Int32                   -- Half area size
instance HasArgument Mosaic "mblend" Int32                  -- Maximum blend size
instance HasArgument Mosaic "bandno" Int32                  -- Band to search for features on
instance HasOutput Mosaic "out" GV.Image                    -- Output image
instance HasOutput Mosaic "dx0" Int32                       -- Detected integer offset
instance HasOutput Mosaic "dy0" Int32                       -- Detected integer offset
instance HasOutput Mosaic "scale1" Double                   -- Detected scale
instance HasOutput Mosaic "angle1" Double                   -- Detected rotation
instance HasOutput Mosaic "dx1" Double                      -- Detected first-order displacement
instance HasOutput Mosaic "dy1" Double                      -- Detected first-order displacement

type Merge = VipsOp "merge" GV.Image
instance HasArgument Merge "ref" GV.Image                   -- Reference image
instance HasArgument Merge "sec" GV.Image                   -- Secondary image
instance HasArgument Merge "direction" GV.Direction         -- Horizontal or vertical merge
instance HasArgument Merge "dx" Int32                       -- Horizontal displacement from sec to ref
instance HasArgument Merge "dy" Int32                       -- Vertical displacement from sec to ref
instance HasArgument Merge "mblend" Int32                   -- Maximum blend size
instance HasOutput Merge "out" GV.Image                     -- Output image

type DrawSmudge = VipsOp "draw_smudge" ()
instance HasArgument DrawSmudge "image" GV.Image            -- Image to draw on
instance HasArgument DrawSmudge "left" Int32                -- Rect to fill
instance HasArgument DrawSmudge "top" Int32                 -- Rect to fill
instance HasArgument DrawSmudge "width" Int32               -- Rect to fill
instance HasArgument DrawSmudge "height" Int32              -- Rect to fill

type DrawImage = VipsOp "draw_image" ()
instance HasArgument DrawImage "image" GV.Image             -- Image to draw on
instance HasArgument DrawImage "sub" GV.Image               -- Sub-image to insert into main image
instance HasArgument DrawImage "x" Int32                    -- Draw image here
instance HasArgument DrawImage "y" Int32                    -- Draw image here
instance HasArgument DrawImage "mode" GV.CombineMode        -- Combining mode

type DrawFlood = VipsOp "draw_flood" DrawFloodResult
data DrawFloodResult = DrawFloodResult { left :: Int32, top :: Int32, width :: Int32, height :: Int32 }
instance HasArgument DrawFlood "image" GV.Image             -- Image to draw on
instance HasArgument DrawFlood "ink" GV.ArrayDouble         -- Color for pixels
instance HasArgument DrawFlood "x" Int32                    -- DrawFlood start point
instance HasArgument DrawFlood "y" Int32                    -- DrawFlood start point
instance HasArgument DrawFlood "test" GV.Image              -- Test pixels in this image
instance HasArgument DrawFlood "equal" Bool                 -- DrawFlood while equal to edge
instance HasOutput DrawFlood "left" Int32                   -- Left edge of modified area
instance HasOutput DrawFlood "top" Int32                    -- top edge of modified area
instance HasOutput DrawFlood "width" Int32                  -- width of modified area
instance HasOutput DrawFlood "height" Int32                 -- height of modified area

type DrawCircle = VipsOp "draw_circle" ()
instance HasArgument DrawCircle "image" GV.Image            -- Image to draw on
instance HasArgument DrawCircle "ink" GV.ArrayDouble        -- Color for pixels
instance HasArgument DrawCircle "cx" Int32                  -- Centre of draw_circle
instance HasArgument DrawCircle "cy" Int32                  -- Centre of draw_circle
instance HasArgument DrawCircle "radius" Int32              -- Radius in pixels
instance HasArgument DrawCircle "fill" Bool                 -- Draw a solid object

type DrawLine = VipsOp "draw_line" ()
instance HasArgument DrawLine "image" GV.Image              -- Image to draw on
instance HasArgument DrawLine "ink" GV.ArrayDouble          -- Color for pixels
instance HasArgument DrawLine "x1" Int32                    -- Start of draw_line
instance HasArgument DrawLine "y1" Int32                    -- Start of draw_line
instance HasArgument DrawLine "x2" Int32                    -- End of draw_line
instance HasArgument DrawLine "y2" Int32                    -- End of draw_line

type DrawMask = VipsOp "draw_mask" ()
instance HasArgument DrawMask "image" GV.Image              -- Image to draw on
instance HasArgument DrawMask "ink" GV.ArrayDouble          -- Color for pixels
instance HasArgument DrawMask "mask" GV.Image               -- Mask of pixels to draw
instance HasArgument DrawMask "x" Int32                     -- Draw mask here
instance HasArgument DrawMask "y" Int32                     -- Draw mask here

type DrawRect = VipsOp "draw_rect" ()
instance HasArgument DrawRect "image" GV.Image              -- Image to draw on
instance HasArgument DrawRect "ink" GV.ArrayDouble          -- Color for pixels
instance HasArgument DrawRect "left" Int32                  -- Rect to fill
instance HasArgument DrawRect "top" Int32                   -- Rect to fill
instance HasArgument DrawRect "width" Int32                 -- Rect to fill
instance HasArgument DrawRect "height" Int32                -- Rect to fill
instance HasArgument DrawRect "fill" Bool                   -- Draw a solid object

type FillNearest = VipsOp "fill_nearest" FillNearestResult
data FillNearestResult = FillNearestResult { out :: GV.Image, distance :: GV.Image }
instance HasArgument FillNearest "in" GV.Image              -- Input image argument
instance HasOutput FillNearest "out" GV.Image               -- Value of nearest non-zero pixel
instance HasOutput FillNearest "distance" GV.Image          -- Distance to nearest non-zero pixel

type Labelregions = VipsOp "labelregions" LabelregionsResult
data LabelregionsResult = LabelregionsResult { mask :: GV.Image, segments :: Int32 }
instance HasArgument Labelregions "in" GV.Image             -- Input image argument
instance HasOutput Labelregions "mask" GV.Image             -- Mask of region labels
instance HasOutput Labelregions "segments" Int32            -- Number of discrete contigious regions

type Countlines = VipsOp "countlines" Double
instance HasArgument Countlines "in" GV.Image               -- Input image argument
instance HasArgument Countlines "direction" GV.Direction    -- Countlines left-right or up-down
instance HasOutput Countlines "nolines" Double              -- Number of lines

type Rank = VipsOp "rank" GV.Image
instance HasArgument Rank "in" GV.Image                     -- Input image argument
instance HasArgument Rank "width" Int32                     -- Window width in pixels
instance HasArgument Rank "height" Int32                    -- Window height in pixels
instance HasArgument Rank "index" Int32                     -- Select pixel at index
instance HasOutput Rank "out" GV.Image                      -- Output image

type Morph = VipsOp "morph" GV.Image
instance HasArgument Morph "in" GV.Image                    -- Input image argument
instance HasArgument Morph "mask" GV.Image                  -- Input matrix image
instance HasArgument Morph "morph" GV.OperationMorphology   -- Morphological operation to perform
instance HasOutput Morph "out" GV.Image                     -- Output image

type Phasecor = VipsOp "phasecor" GV.Image
instance HasArgument Phasecor "in" GV.Image                 -- Input image
instance HasArgument Phasecor "in2" GV.Image                -- Second input image
instance HasOutput Phasecor "out" GV.Image                  -- Output image

type Spectrum = VipsOp "spectrum" GV.Image
instance HasArgument Spectrum "in" GV.Image                 -- Input image
instance HasOutput Spectrum "out" GV.Image                  -- Output image

type Freqmult = VipsOp "freqmult" GV.Image
instance HasArgument Freqmult "in" GV.Image                 -- Input image
instance HasArgument Freqmult "mask" GV.Image               -- Input mask image
instance HasOutput Freqmult "out" GV.Image                  -- Output image

type Invfft = VipsOp "invfft" GV.Image
instance HasArgument Invfft "in" GV.Image                   -- Input image
instance HasArgument Invfft "real" Bool                     -- Output only the real part of the transform
instance HasOutput Invfft "out" GV.Image                    -- Output image

type Fwfft = VipsOp "fwfft" GV.Image
instance HasArgument Fwfft "in" GV.Image                    -- Input image
instance HasOutput Fwfft "out" GV.Image                     -- Output image

type Sobel = VipsOp "sobel" GV.Image
instance HasArgument Sobel "in" GV.Image                    -- Input image
instance HasOutput Sobel "out" GV.Image                     -- Output image

type Canny = VipsOp "canny" GV.Image
instance HasArgument Canny "in" GV.Image                    -- Input image
instance HasArgument Canny "sigma" Double                   -- Sigma of Gaussian
instance HasArgument Canny "precision" GV.Precision         -- Convolve with this precision
instance HasOutput Canny "out" GV.Image                     -- Output image

type Gaussblur = VipsOp "gaussblur" GV.Image
instance HasArgument Gaussblur "in" GV.Image                -- Input image
instance HasArgument Gaussblur "sigma" Double               -- Sigma of Gaussian
instance HasArgument Gaussblur "min-ampl" Double            -- Minimum amplitude of Gaussian
instance HasArgument Gaussblur "precision" GV.Precision     -- Convolve with this precision
instance HasOutput Gaussblur "out" GV.Image                 -- Output image

type Sharpen = VipsOp "sharpen" GV.Image
instance HasArgument Sharpen "in" GV.Image                  -- Input image
instance HasArgument Sharpen "sigma" Double                 -- Sigma of Gaussian
instance HasArgument Sharpen "x1" Double                    -- Flat/jaggy threshold
instance HasArgument Sharpen "y2" Double                    -- Maximum brightening
instance HasArgument Sharpen "y3" Double                    -- Maximum darkening
instance HasArgument Sharpen "m1" Double                    -- Slope for flat areas
instance HasArgument Sharpen "m2" Double                    -- Slope for jaggy areas
instance HasOutput Sharpen "out" GV.Image                   -- Output image

type Spcor = VipsOp "spcor" GV.Image
instance HasArgument Spcor "in" GV.Image                    -- Input image argument
instance HasArgument Spcor "ref" GV.Image                   -- Input reference image
instance HasOutput Spcor "out" GV.Image                     -- Output image

type Fastcor = VipsOp "fastcor" GV.Image
instance HasArgument Fastcor "in" GV.Image                  -- Input image argument
instance HasArgument Fastcor "ref" GV.Image                 -- Input reference image
instance HasOutput Fastcor "out" GV.Image                   -- Output image

type Convasep = VipsOp "convasep" GV.Image
instance HasArgument Convasep "in" GV.Image                 -- Input image argument
instance HasArgument Convasep "mask" GV.Image               -- Input matrix image
instance HasArgument Convasep "layers" Int32                -- Use this many layers in approximation
instance HasOutput Convasep "out" GV.Image                  -- Output image

type Convsep = VipsOp "convsep" GV.Image
instance HasArgument Convsep "in" GV.Image                  -- Input image argument
instance HasArgument Convsep "mask" GV.Image                -- Input matrix image
instance HasArgument Convsep "precision" GV.Precision       -- Convolve with this precision
instance HasArgument Convsep "layers" Int32                 -- Use this many layers in approximation
instance HasArgument Convsep "cluster" Int32                -- Cluster lines closer than this in approximation
instance HasOutput Convsep "out" GV.Image                   -- Output image

type Compass = VipsOp "compass" GV.Image
instance HasArgument Compass "in" GV.Image                  -- Input image argument
instance HasArgument Compass "mask" GV.Image                -- Input matrix image
instance HasArgument Compass "times" Int32                  -- Rotate and convolve this many times
instance HasArgument Compass "angle" GV.Angle45             -- Rotate mask by this much between convolutions
instance HasArgument Compass "combine" GV.Combine           -- Combine convolution results like this
instance HasArgument Compass "precision" GV.Precision       -- Convolve with this precision
instance HasArgument Compass "layers" Int32                 -- Use this many layers in approximation
instance HasArgument Compass "cluster" Int32                -- Cluster lines closer than this in approximation
instance HasOutput Compass "out" GV.Image                   -- Output image

type Convi = VipsOp "convi" GV.Image
instance HasArgument Convi "in" GV.Image                    -- Input image argument
instance HasArgument Convi "mask" GV.Image                  -- Input matrix image
instance HasOutput Convi "out" GV.Image                     -- Output image

type Convf = VipsOp "convf" GV.Image
instance HasArgument Convf "in" GV.Image                    -- Input image argument
instance HasArgument Convf "mask" GV.Image                  -- Input matrix image
instance HasOutput Convf "out" GV.Image                     -- Output image

type Conva = VipsOp "conva" GV.Image
instance HasArgument Conva "in" GV.Image                    -- Input image argument
instance HasArgument Conva "mask" GV.Image                  -- Input matrix image
instance HasArgument Conva "layers" Int32                   -- Use this many layers in approximation
instance HasArgument Conva "cluster" Int32                  -- Cluster lines closer than this in approximation
instance HasOutput Conva "out" GV.Image                     -- Output image

type Conv = VipsOp "conv" GV.Image
instance HasArgument Conv "in" GV.Image                     -- Input image argument
instance HasArgument Conv "mask" GV.Image                   -- Input matrix image
instance HasArgument Conv "precision" GV.Precision          -- Convolve with this precision
instance HasArgument Conv "layers" Int32                    -- Use this many layers in approximation
instance HasArgument Conv "cluster" Int32                   -- Cluster lines closer than this in approximation
instance HasOutput Conv "out" GV.Image                      -- Output image

type HistEntropy = VipsOp "hist_entropy" Double
instance HasArgument HistEntropy "in" GV.Image              -- Input histogram image
instance HasOutput HistEntropy "out" Double                 -- Output value

type HistIsmonotonic = VipsOp "hist_ismonotonic" Bool
instance HasArgument HistIsmonotonic "in" GV.Image          -- Input histogram image
instance HasOutput HistIsmonotonic "monotonic" Bool         -- true if in is monotonic

type HistLocal = VipsOp "hist_local" GV.Image
instance HasArgument HistLocal "in" GV.Image                -- Input image
instance HasArgument HistLocal "width" Int32                -- Window width in pixels
instance HasArgument HistLocal "height" Int32               -- Window height in pixels
instance HasArgument HistLocal "max-slope" Int32            -- Maximum slope (CLAHE)
instance HasOutput HistLocal "out" GV.Image                 -- Output image

type HistPlot = VipsOp "hist_plot" GV.Image
instance HasArgument HistPlot "in" GV.Image                 -- Input image
instance HasOutput HistPlot "out" GV.Image                  -- Output image

type HistEqual = VipsOp "hist_equal" GV.Image
instance HasArgument HistEqual "in" GV.Image                -- Input image
instance HasArgument HistEqual "band" Int32                 -- Equalise with this band
instance HasOutput HistEqual "out" GV.Image                 -- Output image

type HistNorm = VipsOp "hist_norm" GV.Image
instance HasArgument HistNorm "in" GV.Image                 -- Input image
instance HasOutput HistNorm "out" GV.Image                  -- Output image

type HistMatch = VipsOp "hist_match" GV.Image
instance HasArgument HistMatch "in" GV.Image                -- Input histogram
instance HasArgument HistMatch "ref" GV.Image               -- Reference histogram
instance HasOutput HistMatch "out" GV.Image                 -- Output image

type HistCum = VipsOp "hist_cum" GV.Image
instance HasArgument HistCum "in" GV.Image                  -- Input image
instance HasOutput HistCum "out" GV.Image                   -- Output image

type Stdif = VipsOp "stdif" GV.Image
instance HasArgument Stdif "in" GV.Image                    -- Input image
instance HasArgument Stdif "a" Double                       -- Weight of new mean
instance HasArgument Stdif "m0" Double                      -- New mean
instance HasArgument Stdif "b" Double                       -- Weight of new deviation
instance HasArgument Stdif "s0" Double                      -- New deviation
instance HasArgument Stdif "width" Int32                    -- Window width in pixels
instance HasArgument Stdif "height" Int32                   -- Window height in pixels
instance HasOutput Stdif "out" GV.Image                     -- Output image

type Percent = VipsOp "percent" Int32
instance HasArgument Percent "in" GV.Image                  -- Input image
instance HasArgument Percent "percent" Double               -- Percent of pixels
instance HasOutput Percent "threshold" Int32                -- Threshold above which lie percent of pixels

type Case = VipsOp "case" GV.Image
instance HasArgument Case "index" GV.Image                  -- Index image
instance HasArgument Case "cases" GV.ArrayImage             -- Array of case images
instance HasOutput Case "out" GV.Image                      -- Output image

type Maplut = VipsOp "maplut" GV.Image
instance HasArgument Maplut "in" GV.Image                   -- Input image
instance HasArgument Maplut "lut" GV.Image                  -- Look-up table image
instance HasArgument Maplut "band" Int32                    -- apply one-band lut to this band of in
instance HasOutput Maplut "out" GV.Image                    -- Output image

type ProfileLoad = VipsOp "profile_load" GV.Blob
instance HasArgument ProfileLoad "name" T.Text              -- Profile name
instance HasOutput ProfileLoad "profile" GV.Blob            -- Loaded profile

type XyZ2Cmyk = VipsOp "XYZ2CMYK" GV.Image
instance HasArgument XyZ2Cmyk "in" GV.Image                 -- Input image
instance HasOutput XyZ2Cmyk "out" GV.Image                  -- Output image

type CmyK2Xyz = VipsOp "CMYK2XYZ" GV.Image
instance HasArgument CmyK2Xyz "in" GV.Image                 -- Input image
instance HasOutput CmyK2Xyz "out" GV.Image                  -- Output image

type ScRgB2sRgb = VipsOp "scRGB2sRGB" GV.Image
instance HasArgument ScRgB2sRgb "in" GV.Image               -- Input image
instance HasArgument ScRgB2sRgb "depth" Int32               -- Output device space depth in bits
instance HasOutput ScRgB2sRgb "out" GV.Image                -- Output image

type ScRgB2Bw = VipsOp "scRGB2BW" GV.Image
instance HasArgument ScRgB2Bw "in" GV.Image                 -- Input image
instance HasArgument ScRgB2Bw "depth" Int32                 -- Output device space depth in bits
instance HasOutput ScRgB2Bw "out" GV.Image                  -- Output image

type SRgB2scRgb = VipsOp "sRGB2scRGB" GV.Image
instance HasArgument SRgB2scRgb "in" GV.Image               -- Input image
instance HasOutput SRgB2scRgb "out" GV.Image                -- Output image

type DEcmc = VipsOp "dECMC" GV.Image
instance HasArgument DEcmc "left" GV.Image                  -- Left-hand input image
instance HasArgument DEcmc "right" GV.Image                 -- Right-hand input image
instance HasOutput DEcmc "out" GV.Image                     -- Output image

type DE00 = VipsOp "dE00" GV.Image
instance HasArgument DE00 "left" GV.Image                   -- Left-hand input image
instance HasArgument DE00 "right" GV.Image                  -- Right-hand input image
instance HasOutput DE00 "out" GV.Image                      -- Output image

type DE76 = VipsOp "dE76" GV.Image
instance HasArgument DE76 "left" GV.Image                   -- Left-hand input image
instance HasArgument DE76 "right" GV.Image                  -- Right-hand input image
instance HasOutput DE76 "out" GV.Image                      -- Output image

type IccTransform = VipsOp "icc_transform" GV.Image
instance HasArgument IccTransform "in" GV.Image             -- Input image
instance HasArgument IccTransform "intent" GV.Intent        -- Rendering intent
instance HasArgument IccTransform "pcs" GV.PCS              -- Set Profile Connection Space
instance HasArgument IccTransform "output-profile" T.Text   -- Filename to load output profile from
instance HasArgument IccTransform "embedded" Bool           -- Use embedded input profile, if available
instance HasArgument IccTransform "input-profile" T.Text    -- Filename to load input profile from
instance HasArgument IccTransform "depth" Int32             -- Output device space depth in bits
instance HasOutput IccTransform "out" GV.Image              -- Output image

type IccExport = VipsOp "icc_export" GV.Image
instance HasArgument IccExport "in" GV.Image                -- Input image
instance HasArgument IccExport "intent" GV.Intent           -- Rendering intent
instance HasArgument IccExport "pcs" GV.PCS                 -- Set Profile Connection Space
instance HasArgument IccExport "output-profile" T.Text      -- Filename to load output profile from
instance HasArgument IccExport "depth" Int32                -- Output device space depth in bits
instance HasOutput IccExport "out" GV.Image                 -- Output image

type IccImport = VipsOp "icc_import" GV.Image
instance HasArgument IccImport "in" GV.Image                -- Input image
instance HasArgument IccImport "intent" GV.Intent           -- Rendering intent
instance HasArgument IccImport "pcs" GV.PCS                 -- Set Profile Connection Space
instance HasArgument IccImport "embedded" Bool              -- Use embedded input profile, if available
instance HasArgument IccImport "input-profile" T.Text       -- Filename to load input profile from
instance HasOutput IccImport "out" GV.Image                 -- Output image

type HsV2sRgb = VipsOp "HSV2sRGB" GV.Image
instance HasArgument HsV2sRgb "in" GV.Image                 -- Input image
instance HasOutput HsV2sRgb "out" GV.Image                  -- Output image

type SRgB2Hsv = VipsOp "sRGB2HSV" GV.Image
instance HasArgument SRgB2Hsv "in" GV.Image                 -- Input image
instance HasOutput SRgB2Hsv "out" GV.Image                  -- Output image

type LabQ2sRgb = VipsOp "LabQ2sRGB" GV.Image
instance HasArgument LabQ2sRgb "in" GV.Image                -- Input image
instance HasOutput LabQ2sRgb "out" GV.Image                 -- Output image

type Float2rad = VipsOp "float2rad" GV.Image
instance HasArgument Float2rad "in" GV.Image                -- Input image
instance HasOutput Float2rad "out" GV.Image                 -- Output image

type Rad2float = VipsOp "rad2float" GV.Image
instance HasArgument Rad2float "in" GV.Image                -- Input image
instance HasOutput Rad2float "out" GV.Image                 -- Output image

type Lab2LabS = VipsOp "Lab2LabS" GV.Image
instance HasArgument Lab2LabS "in" GV.Image                 -- Input image
instance HasOutput Lab2LabS "out" GV.Image                  -- Output image

type LabS2Lab = VipsOp "LabS2Lab" GV.Image
instance HasArgument LabS2Lab "in" GV.Image                 -- Input image
instance HasOutput LabS2Lab "out" GV.Image                  -- Output image

type LabS2LabQ = VipsOp "LabS2LabQ" GV.Image
instance HasArgument LabS2LabQ "in" GV.Image                -- Input image
instance HasOutput LabS2LabQ "out" GV.Image                 -- Output image

type LabQ2LabS = VipsOp "LabQ2LabS" GV.Image
instance HasArgument LabQ2LabS "in" GV.Image                -- Input image
instance HasOutput LabQ2LabS "out" GV.Image                 -- Output image

type Lab2LabQ = VipsOp "Lab2LabQ" GV.Image
instance HasArgument Lab2LabQ "in" GV.Image                 -- Input image
instance HasOutput Lab2LabQ "out" GV.Image                  -- Output image

type LabQ2Lab = VipsOp "LabQ2Lab" GV.Image
instance HasArgument LabQ2Lab "in" GV.Image                 -- Input image
instance HasOutput LabQ2Lab "out" GV.Image                  -- Output image

type XyZ2scRgb = VipsOp "XYZ2scRGB" GV.Image
instance HasArgument XyZ2scRgb "in" GV.Image                -- Input image
instance HasOutput XyZ2scRgb "out" GV.Image                 -- Output image

type ScRgB2Xyz = VipsOp "scRGB2XYZ" GV.Image
instance HasArgument ScRgB2Xyz "in" GV.Image                -- Input image
instance HasOutput ScRgB2Xyz "out" GV.Image                 -- Output image

type Yxy2Xyz = VipsOp "Yxy2XYZ" GV.Image
instance HasArgument Yxy2Xyz "in" GV.Image                  -- Input image
instance HasOutput Yxy2Xyz "out" GV.Image                   -- Output image

type XyZ2Yxy = VipsOp "XYZ2Yxy" GV.Image
instance HasArgument XyZ2Yxy "in" GV.Image                  -- Input image
instance HasOutput XyZ2Yxy "out" GV.Image                   -- Output image

type CmC2LCh = VipsOp "CMC2LCh" GV.Image
instance HasArgument CmC2LCh "in" GV.Image                  -- Input image
instance HasOutput CmC2LCh "out" GV.Image                   -- Output image

type LCh2Cmc = VipsOp "LCh2CMC" GV.Image
instance HasArgument LCh2Cmc "in" GV.Image                  -- Input image
instance HasOutput LCh2Cmc "out" GV.Image                   -- Output image

type LCh2Lab = VipsOp "LCh2Lab" GV.Image
instance HasArgument LCh2Lab "in" GV.Image                  -- Input image
instance HasOutput LCh2Lab "out" GV.Image                   -- Output image

type Lab2LCh = VipsOp "Lab2LCh" GV.Image
instance HasArgument Lab2LCh "in" GV.Image                  -- Input image
instance HasOutput Lab2LCh "out" GV.Image                   -- Output image

type XyZ2Lab = VipsOp "XYZ2Lab" GV.Image
instance HasArgument XyZ2Lab "in" GV.Image                  -- Input image
instance HasArgument XyZ2Lab "temp" GV.ArrayDouble          -- Colour temperature
instance HasOutput XyZ2Lab "out" GV.Image                   -- Output image

type Lab2Xyz = VipsOp "Lab2XYZ" GV.Image
instance HasArgument Lab2Xyz "in" GV.Image                  -- Input image
instance HasArgument Lab2Xyz "temp" GV.ArrayDouble          -- Color temperature
instance HasOutput Lab2Xyz "out" GV.Image                   -- Output image

type Colourspace = VipsOp "colourspace" GV.Image
instance HasArgument Colourspace "in" GV.Image              -- Input image
instance HasArgument Colourspace "space" GV.Interpretation  -- Destination color space
instance HasArgument Colourspace "source-space" GV.Interpretation  -- Source color space
instance HasOutput Colourspace "out" GV.Image               -- Output image

type Resize = VipsOp "resize" GV.Image
instance HasArgument Resize "in" GV.Image                   -- Input image argument
instance HasArgument Resize "kernel" GV.Kernel              -- Resampling kernel
instance HasArgument Resize "scale" Double                  -- Scale image by this factor
instance HasArgument Resize "vscale" Double                 -- Vertical scale image by this factor
instance HasOutput Resize "out" GV.Image                    -- Output image

type Rotate = VipsOp "rotate" GV.Image
instance HasArgument Rotate "in" GV.Image                   -- Input image argument
instance HasArgument Rotate "angle" Double                  -- Rotate anticlockwise by this many degrees
instance HasArgument Rotate "interpolate" GV.Interpolate    -- Interpolate pixels with this
instance HasArgument Rotate "background" GV.ArrayDouble     -- Background value
instance HasArgument Rotate "odx" Double                    -- Horizontal output displacement
instance HasArgument Rotate "ody" Double                    -- Vertical output displacement
instance HasArgument Rotate "idx" Double                    -- Horizontal input displacement
instance HasArgument Rotate "idy" Double                    -- Vertical input displacement
instance HasOutput Rotate "out" GV.Image                    -- Output image

type Similarity = VipsOp "similarity" GV.Image
instance HasArgument Similarity "in" GV.Image               -- Input image argument
instance HasArgument Similarity "scale" Double              -- Scale by this factor
instance HasArgument Similarity "angle" Double              -- Rotate anticlockwise by this many degrees
instance HasArgument Similarity "interpolate" GV.Interpolate  -- Interpolate pixels with this
instance HasArgument Similarity "background" GV.ArrayDouble  -- Background value
instance HasArgument Similarity "odx" Double                -- Horizontal output displacement
instance HasArgument Similarity "ody" Double                -- Vertical output displacement
instance HasArgument Similarity "idx" Double                -- Horizontal input displacement
instance HasArgument Similarity "idy" Double                -- Vertical input displacement
instance HasOutput Similarity "out" GV.Image                -- Output image

type Affine = VipsOp "affine" GV.Image
instance HasArgument Affine "in" GV.Image                   -- Input image argument
instance HasArgument Affine "interpolate" GV.Interpolate    -- Interpolate pixels with this
instance HasArgument Affine "matrix" GV.ArrayDouble         -- Transformation matrix
instance HasArgument Affine "oarea" GV.ArrayInt             -- Area of output to generate
instance HasArgument Affine "odx" Double                    -- Horizontal output displacement
instance HasArgument Affine "ody" Double                    -- Vertical output displacement
instance HasArgument Affine "idx" Double                    -- Horizontal input displacement
instance HasArgument Affine "idy" Double                    -- Vertical input displacement
instance HasArgument Affine "background" GV.ArrayDouble     -- Background value
instance HasArgument Affine "extend" GV.Extend              -- How to generate the extra pixels
instance HasArgument Affine "premultiplied" Bool            -- Images have premultiplied alpha
instance HasOutput Affine "out" GV.Image                    -- Output image

type Quadratic = VipsOp "quadratic" GV.Image
instance HasArgument Quadratic "in" GV.Image                -- Input image argument
instance HasArgument Quadratic "coeff" GV.Image             -- Coefficient matrix
instance HasArgument Quadratic "interpolate" GV.Interpolate  -- Interpolate values with this
instance HasOutput Quadratic "out" GV.Image                 -- Output image

type Reduce = VipsOp "reduce" GV.Image
instance HasArgument Reduce "in" GV.Image                   -- Input image argument
instance HasArgument Reduce "kernel" GV.Kernel              -- Resampling kernel
instance HasArgument Reduce "hshrink" Double                -- Horizontal shrink factor
instance HasArgument Reduce "vshrink" Double                -- Vertical shrink factor
instance HasOutput Reduce "out" GV.Image                    -- Output image

type Reducev = VipsOp "reducev" GV.Image
instance HasArgument Reducev "in" GV.Image                  -- Input image argument
instance HasArgument Reducev "vshrink" Double               -- Vertical shrink factor
instance HasArgument Reducev "kernel" GV.Kernel             -- Resampling kernel
instance HasOutput Reducev "out" GV.Image                   -- Output image

type Reduceh = VipsOp "reduceh" GV.Image
instance HasArgument Reduceh "in" GV.Image                  -- Input image argument
instance HasArgument Reduceh "hshrink" Double               -- Horizontal shrink factor
instance HasArgument Reduceh "kernel" GV.Kernel             -- Resampling kernel
instance HasOutput Reduceh "out" GV.Image                   -- Output image

type Shrinkv = VipsOp "shrinkv" GV.Image
instance HasArgument Shrinkv "in" GV.Image                  -- Input image argument
instance HasArgument Shrinkv "vshrink" Int32                -- Vertical shrink factor
instance HasOutput Shrinkv "out" GV.Image                   -- Output image

type Shrinkh = VipsOp "shrinkh" GV.Image
instance HasArgument Shrinkh "in" GV.Image                  -- Input image argument
instance HasArgument Shrinkh "hshrink" Int32                -- Horizontal shrink factor
instance HasOutput Shrinkh "out" GV.Image                   -- Output image

type Shrink = VipsOp "shrink" GV.Image
instance HasArgument Shrink "in" GV.Image                   -- Input image argument
instance HasArgument Shrink "hshrink" Double                -- Horizontal shrink factor
instance HasArgument Shrink "vshrink" Double                -- Vertical shrink factor
instance HasOutput Shrink "out" GV.Image                    -- Output image

type Mapim = VipsOp "mapim" GV.Image
instance HasArgument Mapim "in" GV.Image                    -- Input image argument
instance HasArgument Mapim "index" GV.Image                 -- Index pixels with this
instance HasArgument Mapim "interpolate" GV.Interpolate     -- Interpolate pixels with this
instance HasOutput Mapim "out" GV.Image                     -- Output image

type ThumbnailSource = VipsOp "thumbnail_source" GV.Image
instance HasArgument ThumbnailSource "source" VipsSource    -- Source to load from
instance HasArgument ThumbnailSource "width" Int32          -- Size to this width
instance HasArgument ThumbnailSource "option-string" T.Text  -- Options that are passed on to the underlying loader
instance HasArgument ThumbnailSource "height" Int32         -- Size to this height
instance HasArgument ThumbnailSource "size" GV.Size         -- Only upsize, only downsize, or both
instance HasArgument ThumbnailSource "no-rotate" Bool       -- Don't use orientation tags to rotate image upright
instance HasArgument ThumbnailSource "crop" GV.Interesting  -- Reduce to fill target rectangle, then crop
instance HasArgument ThumbnailSource "linear" Bool          -- Reduce in linear light
instance HasArgument ThumbnailSource "import-profile" T.Text  -- Fallback import profile
instance HasArgument ThumbnailSource "export-profile" T.Text  -- Fallback export profile
instance HasArgument ThumbnailSource "intent" GV.Intent     -- Rendering intent
instance HasOutput ThumbnailSource "out" GV.Image           -- Output image

type ThumbnailImage = VipsOp "thumbnail_image" GV.Image
instance HasArgument ThumbnailImage "in" GV.Image           -- Input image argument
instance HasArgument ThumbnailImage "width" Int32           -- Size to this width
instance HasArgument ThumbnailImage "height" Int32          -- Size to this height
instance HasArgument ThumbnailImage "size" GV.Size          -- Only upsize, only downsize, or both
instance HasArgument ThumbnailImage "no-rotate" Bool        -- Don't use orientation tags to rotate image upright
instance HasArgument ThumbnailImage "crop" GV.Interesting   -- Reduce to fill target rectangle, then crop
instance HasArgument ThumbnailImage "linear" Bool           -- Reduce in linear light
instance HasArgument ThumbnailImage "import-profile" T.Text  -- Fallback import profile
instance HasArgument ThumbnailImage "export-profile" T.Text  -- Fallback export profile
instance HasArgument ThumbnailImage "intent" GV.Intent      -- Rendering intent
instance HasOutput ThumbnailImage "out" GV.Image            -- Output image

type ThumbnailBuffer = VipsOp "thumbnail_buffer" GV.Image
instance HasArgument ThumbnailBuffer "buffer" GV.Blob       -- Buffer to load from
instance HasArgument ThumbnailBuffer "width" Int32          -- Size to this width
instance HasArgument ThumbnailBuffer "option-string" T.Text  -- Options that are passed on to the underlying loader
instance HasArgument ThumbnailBuffer "height" Int32         -- Size to this height
instance HasArgument ThumbnailBuffer "size" GV.Size         -- Only upsize, only downsize, or both
instance HasArgument ThumbnailBuffer "no-rotate" Bool       -- Don't use orientation tags to rotate image upright
instance HasArgument ThumbnailBuffer "crop" GV.Interesting  -- Reduce to fill target rectangle, then crop
instance HasArgument ThumbnailBuffer "linear" Bool          -- Reduce in linear light
instance HasArgument ThumbnailBuffer "import-profile" T.Text  -- Fallback import profile
instance HasArgument ThumbnailBuffer "export-profile" T.Text  -- Fallback export profile
instance HasArgument ThumbnailBuffer "intent" GV.Intent     -- Rendering intent
instance HasOutput ThumbnailBuffer "out" GV.Image           -- Output image

type Thumbnail = VipsOp "thumbnail" GV.Image
instance HasArgument Thumbnail "filename" T.Text            -- Filename to read from
instance HasArgument Thumbnail "width" Int32                -- Size to this width
instance HasArgument Thumbnail "height" Int32               -- Size to this height
instance HasArgument Thumbnail "size" GV.Size               -- Only upsize, only downsize, or both
instance HasArgument Thumbnail "no-rotate" Bool             -- Don't use orientation tags to rotate image upright
instance HasArgument Thumbnail "crop" GV.Interesting        -- Reduce to fill target rectangle, then crop
instance HasArgument Thumbnail "linear" Bool                -- Reduce in linear light
instance HasArgument Thumbnail "import-profile" T.Text      -- Fallback import profile
instance HasArgument Thumbnail "export-profile" T.Text      -- Fallback export profile
instance HasArgument Thumbnail "intent" GV.Intent           -- Rendering intent
instance HasOutput Thumbnail "out" GV.Image                 -- Output image

type HeifsaveTarget = VipsOp "heifsave_target" ()
instance HasArgument HeifsaveTarget "in" GV.Image           -- Image to save
instance HasArgument HeifsaveTarget "target" VipsTarget     -- Target to save to
instance HasArgument HeifsaveTarget "Q" Int32               -- Q factor
instance HasArgument HeifsaveTarget "lossless" Bool         -- Enable lossless compression
instance HasArgument HeifsaveTarget "compression" GV.ForeignHeifCompression  -- Compression format
instance HasArgument HeifsaveTarget "speed" Int32           -- CPU effort
instance HasArgument HeifsaveTarget "strip" Bool            -- Strip all metadata from image
instance HasArgument HeifsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument HeifsaveTarget "page-height" Int32     -- Set page height for multipage save

type HeifsaveBuffer = VipsOp "heifsave_buffer" GV.Blob
instance HasArgument HeifsaveBuffer "in" GV.Image           -- Image to save
instance HasArgument HeifsaveBuffer "Q" Int32               -- Q factor
instance HasArgument HeifsaveBuffer "lossless" Bool         -- Enable lossless compression
instance HasArgument HeifsaveBuffer "compression" GV.ForeignHeifCompression  -- Compression format
instance HasArgument HeifsaveBuffer "speed" Int32           -- CPU effort
instance HasArgument HeifsaveBuffer "strip" Bool            -- Strip all metadata from image
instance HasArgument HeifsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument HeifsaveBuffer "page-height" Int32     -- Set page height for multipage save
instance HasOutput HeifsaveBuffer "buffer" GV.Blob          -- Buffer to save to

type Heifsave = VipsOp "heifsave" ()
instance HasArgument Heifsave "in" GV.Image                 -- Image to save
instance HasArgument Heifsave "filename" T.Text             -- Filename to load from
instance HasArgument Heifsave "Q" Int32                     -- Q factor
instance HasArgument Heifsave "lossless" Bool               -- Enable lossless compression
instance HasArgument Heifsave "compression" GV.ForeignHeifCompression  -- Compression format
instance HasArgument Heifsave "speed" Int32                 -- CPU effort
instance HasArgument Heifsave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Heifsave "background" GV.ArrayDouble   -- Background value
instance HasArgument Heifsave "page-height" Int32           -- Set page height for multipage save

type Fitssave = VipsOp "fitssave" ()
instance HasArgument Fitssave "in" GV.Image                 -- Image to save
instance HasArgument Fitssave "filename" T.Text             -- Filename to save to
instance HasArgument Fitssave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Fitssave "background" GV.ArrayDouble   -- Background value
instance HasArgument Fitssave "page-height" Int32           -- Set page height for multipage save

type MagicksaveBuffer = VipsOp "magicksave_buffer" GV.Blob
instance HasArgument MagicksaveBuffer "in" GV.Image         -- Image to save
instance HasArgument MagicksaveBuffer "format" T.Text       -- Format to save in
instance HasArgument MagicksaveBuffer "quality" Int32       -- Quality to use
instance HasArgument MagicksaveBuffer "optimize-gif-frames" Bool  -- Apply GIF frames optimization
instance HasArgument MagicksaveBuffer "optimize-gif-transparency" Bool  -- Apply GIF transparency optimization
instance HasArgument MagicksaveBuffer "strip" Bool          -- Strip all metadata from image
instance HasArgument MagicksaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument MagicksaveBuffer "page-height" Int32   -- Set page height for multipage save
instance HasOutput MagicksaveBuffer "buffer" GV.Blob        -- Buffer to save to

type Magicksave = VipsOp "magicksave" ()
instance HasArgument Magicksave "in" GV.Image               -- Image to save
instance HasArgument Magicksave "filename" T.Text           -- Filename to save to
instance HasArgument Magicksave "format" T.Text             -- Format to save in
instance HasArgument Magicksave "quality" Int32             -- Quality to use
instance HasArgument Magicksave "optimize-gif-frames" Bool  -- Apply GIF frames optimization
instance HasArgument Magicksave "optimize-gif-transparency" Bool  -- Apply GIF transparency optimization
instance HasArgument Magicksave "strip" Bool                -- Strip all metadata from image
instance HasArgument Magicksave "background" GV.ArrayDouble  -- Background value
instance HasArgument Magicksave "page-height" Int32         -- Set page height for multipage save

type TiffsaveBuffer = VipsOp "tiffsave_buffer" GV.Blob
instance HasArgument TiffsaveBuffer "in" GV.Image           -- Image to save
instance HasArgument TiffsaveBuffer "compression" GV.ForeignTiffCompression  -- Compression for this file
instance HasArgument TiffsaveBuffer "Q" Int32               -- Q factor
instance HasArgument TiffsaveBuffer "predictor" GV.ForeignTiffPredictor  -- Compression prediction
instance HasArgument TiffsaveBuffer "profile" T.Text        -- ICC profile to embed
instance HasArgument TiffsaveBuffer "tile" Bool             -- Write a tiled tiff
instance HasArgument TiffsaveBuffer "tile-width" Int32      -- Tile width in pixels
instance HasArgument TiffsaveBuffer "tile-height" Int32     -- Tile height in pixels
instance HasArgument TiffsaveBuffer "pyramid" Bool          -- Write a pyramidal tiff
instance HasArgument TiffsaveBuffer "miniswhite" Bool       -- Use 0 for white in 1-bit images
instance HasArgument TiffsaveBuffer "bitdepth" Int32        -- Write as a 1, 2, 4 or 8 bit image
instance HasArgument TiffsaveBuffer "resunit" GV.ForeignTiffResunit  -- Resolution unit
instance HasArgument TiffsaveBuffer "xres" Double           -- Horizontal resolution in pixels/mm
instance HasArgument TiffsaveBuffer "yres" Double           -- Vertical resolution in pixels/mm
instance HasArgument TiffsaveBuffer "bigtiff" Bool          -- Write a bigtiff image
instance HasArgument TiffsaveBuffer "properties" Bool       -- Write a properties document to IMAGEDESCRIPTION
instance HasArgument TiffsaveBuffer "region-shrink" GV.RegionShrink  -- Method to shrink regions
instance HasArgument TiffsaveBuffer "level" Int32           -- ZSTD compression level
instance HasArgument TiffsaveBuffer "lossless" Bool         -- Enable WEBP lossless mode
instance HasArgument TiffsaveBuffer "subifd" Bool           -- Save pyr layers as sub-IFDs
instance HasArgument TiffsaveBuffer "depth" GV.ForeignDzDepth  -- Pyramid depth
instance HasArgument TiffsaveBuffer "strip" Bool            -- Strip all metadata from image
instance HasArgument TiffsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument TiffsaveBuffer "page-height" Int32     -- Set page height for multipage save
instance HasOutput TiffsaveBuffer "buffer" GV.Blob          -- Buffer to save to

type Tiffsave = VipsOp "tiffsave" ()
instance HasArgument Tiffsave "in" GV.Image                 -- Image to save
instance HasArgument Tiffsave "filename" T.Text             -- Filename to save to
instance HasArgument Tiffsave "compression" GV.ForeignTiffCompression  -- Compression for this file
instance HasArgument Tiffsave "Q" Int32                     -- Q factor
instance HasArgument Tiffsave "predictor" GV.ForeignTiffPredictor  -- Compression prediction
instance HasArgument Tiffsave "profile" T.Text              -- ICC profile to embed
instance HasArgument Tiffsave "tile" Bool                   -- Write a tiled tiff
instance HasArgument Tiffsave "tile-width" Int32            -- Tile width in pixels
instance HasArgument Tiffsave "tile-height" Int32           -- Tile height in pixels
instance HasArgument Tiffsave "pyramid" Bool                -- Write a pyramidal tiff
instance HasArgument Tiffsave "miniswhite" Bool             -- Use 0 for white in 1-bit images
instance HasArgument Tiffsave "bitdepth" Int32              -- Write as a 1, 2, 4 or 8 bit image
instance HasArgument Tiffsave "resunit" GV.ForeignTiffResunit  -- Resolution unit
instance HasArgument Tiffsave "xres" Double                 -- Horizontal resolution in pixels/mm
instance HasArgument Tiffsave "yres" Double                 -- Vertical resolution in pixels/mm
instance HasArgument Tiffsave "bigtiff" Bool                -- Write a bigtiff image
instance HasArgument Tiffsave "properties" Bool             -- Write a properties document to IMAGEDESCRIPTION
instance HasArgument Tiffsave "region-shrink" GV.RegionShrink  -- Method to shrink regions
instance HasArgument Tiffsave "level" Int32                 -- ZSTD compression level
instance HasArgument Tiffsave "lossless" Bool               -- Enable WEBP lossless mode
instance HasArgument Tiffsave "subifd" Bool                 -- Save pyr layers as sub-IFDs
instance HasArgument Tiffsave "depth" GV.ForeignDzDepth     -- Pyramid depth
instance HasArgument Tiffsave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Tiffsave "background" GV.ArrayDouble   -- Background value
instance HasArgument Tiffsave "page-height" Int32           -- Set page height for multipage save

type WebpsaveTarget = VipsOp "webpsave_target" ()
instance HasArgument WebpsaveTarget "in" GV.Image           -- Image to save
instance HasArgument WebpsaveTarget "target" VipsTarget     -- Target to save to
instance HasArgument WebpsaveTarget "Q" Int32               -- Q factor
instance HasArgument WebpsaveTarget "lossless" Bool         -- enable lossless compression
instance HasArgument WebpsaveTarget "preset" GV.ForeignWebpPreset  -- Preset for lossy compression
instance HasArgument WebpsaveTarget "smart-subsample" Bool  -- Enable high quality chroma subsampling
instance HasArgument WebpsaveTarget "near-lossless" Bool    -- Enable preprocessing in lossless mode (uses Q)
instance HasArgument WebpsaveTarget "alpha-q" Int32         -- Change alpha plane fidelity for lossy compression
instance HasArgument WebpsaveTarget "min-size" Bool         -- Optimise for minium size
instance HasArgument WebpsaveTarget "kmin" Int32            -- Minimum number of frames between key frames
instance HasArgument WebpsaveTarget "kmax" Int32            -- Maximum number of frames between key frames
instance HasArgument WebpsaveTarget "reduction-effort" Int32  -- Level of CPU effort to reduce file size
instance HasArgument WebpsaveTarget "profile" T.Text        -- ICC profile to embed
instance HasArgument WebpsaveTarget "strip" Bool            -- Strip all metadata from image
instance HasArgument WebpsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument WebpsaveTarget "page-height" Int32     -- Set page height for multipage save

type WebpsaveBuffer = VipsOp "webpsave_buffer" GV.Blob
instance HasArgument WebpsaveBuffer "in" GV.Image           -- Image to save
instance HasArgument WebpsaveBuffer "Q" Int32               -- Q factor
instance HasArgument WebpsaveBuffer "lossless" Bool         -- enable lossless compression
instance HasArgument WebpsaveBuffer "preset" GV.ForeignWebpPreset  -- Preset for lossy compression
instance HasArgument WebpsaveBuffer "smart-subsample" Bool  -- Enable high quality chroma subsampling
instance HasArgument WebpsaveBuffer "near-lossless" Bool    -- Enable preprocessing in lossless mode (uses Q)
instance HasArgument WebpsaveBuffer "alpha-q" Int32         -- Change alpha plane fidelity for lossy compression
instance HasArgument WebpsaveBuffer "min-size" Bool         -- Optimise for minium size
instance HasArgument WebpsaveBuffer "kmin" Int32            -- Minimum number of frames between key frames
instance HasArgument WebpsaveBuffer "kmax" Int32            -- Maximum number of frames between key frames
instance HasArgument WebpsaveBuffer "reduction-effort" Int32  -- Level of CPU effort to reduce file size
instance HasArgument WebpsaveBuffer "profile" T.Text        -- ICC profile to embed
instance HasArgument WebpsaveBuffer "strip" Bool            -- Strip all metadata from image
instance HasArgument WebpsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument WebpsaveBuffer "page-height" Int32     -- Set page height for multipage save
instance HasOutput WebpsaveBuffer "buffer" GV.Blob          -- Buffer to save to

type Webpsave = VipsOp "webpsave" ()
instance HasArgument Webpsave "in" GV.Image                 -- Image to save
instance HasArgument Webpsave "filename" T.Text             -- Filename to save to
instance HasArgument Webpsave "Q" Int32                     -- Q factor
instance HasArgument Webpsave "lossless" Bool               -- enable lossless compression
instance HasArgument Webpsave "preset" GV.ForeignWebpPreset  -- Preset for lossy compression
instance HasArgument Webpsave "smart-subsample" Bool        -- Enable high quality chroma subsampling
instance HasArgument Webpsave "near-lossless" Bool          -- Enable preprocessing in lossless mode (uses Q)
instance HasArgument Webpsave "alpha-q" Int32               -- Change alpha plane fidelity for lossy compression
instance HasArgument Webpsave "min-size" Bool               -- Optimise for minium size
instance HasArgument Webpsave "kmin" Int32                  -- Minimum number of frames between key frames
instance HasArgument Webpsave "kmax" Int32                  -- Maximum number of frames between key frames
instance HasArgument Webpsave "reduction-effort" Int32      -- Level of CPU effort to reduce file size
instance HasArgument Webpsave "profile" T.Text              -- ICC profile to embed
instance HasArgument Webpsave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Webpsave "background" GV.ArrayDouble   -- Background value
instance HasArgument Webpsave "page-height" Int32           -- Set page height for multipage save

type JpegsaveMime = VipsOp "jpegsave_mime" ()
instance HasArgument JpegsaveMime "in" GV.Image             -- Image to save
instance HasArgument JpegsaveMime "Q" Int32                 -- Q factor
instance HasArgument JpegsaveMime "profile" T.Text          -- ICC profile to embed
instance HasArgument JpegsaveMime "optimize-coding" Bool    -- Compute optimal Huffman coding tables
instance HasArgument JpegsaveMime "interlace" Bool          -- Generate an interlaced (progressive) jpeg
instance HasArgument JpegsaveMime "trellis-quant" Bool      -- Apply trellis quantisation to each 8x8 block
instance HasArgument JpegsaveMime "overshoot-deringing" Bool  -- Apply overshooting to samples with extreme values
instance HasArgument JpegsaveMime "optimize-scans" Bool     -- Split spectrum of DCT coefficients into separate scans
instance HasArgument JpegsaveMime "quant-table" Int32       -- Use predefined quantization table with given index
instance HasArgument JpegsaveMime "subsample-mode" GV.ForeignJpegSubsample  -- Select chroma subsample operation mode
instance HasArgument JpegsaveMime "strip" Bool              -- Strip all metadata from image
instance HasArgument JpegsaveMime "background" GV.ArrayDouble  -- Background value
instance HasArgument JpegsaveMime "page-height" Int32       -- Set page height for multipage save

type JpegsaveTarget = VipsOp "jpegsave_target" ()
instance HasArgument JpegsaveTarget "in" GV.Image           -- Image to save
instance HasArgument JpegsaveTarget "target" VipsTarget     -- Target to save to
instance HasArgument JpegsaveTarget "Q" Int32               -- Q factor
instance HasArgument JpegsaveTarget "profile" T.Text        -- ICC profile to embed
instance HasArgument JpegsaveTarget "optimize-coding" Bool  -- Compute optimal Huffman coding tables
instance HasArgument JpegsaveTarget "interlace" Bool        -- Generate an interlaced (progressive) jpeg
instance HasArgument JpegsaveTarget "trellis-quant" Bool    -- Apply trellis quantisation to each 8x8 block
instance HasArgument JpegsaveTarget "overshoot-deringing" Bool  -- Apply overshooting to samples with extreme values
instance HasArgument JpegsaveTarget "optimize-scans" Bool   -- Split spectrum of DCT coefficients into separate scans
instance HasArgument JpegsaveTarget "quant-table" Int32     -- Use predefined quantization table with given index
instance HasArgument JpegsaveTarget "subsample-mode" GV.ForeignJpegSubsample  -- Select chroma subsample operation mode
instance HasArgument JpegsaveTarget "strip" Bool            -- Strip all metadata from image
instance HasArgument JpegsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument JpegsaveTarget "page-height" Int32     -- Set page height for multipage save

type JpegsaveBuffer = VipsOp "jpegsave_buffer" GV.Blob
instance HasArgument JpegsaveBuffer "in" GV.Image           -- Image to save
instance HasArgument JpegsaveBuffer "Q" Int32               -- Q factor
instance HasArgument JpegsaveBuffer "profile" T.Text        -- ICC profile to embed
instance HasArgument JpegsaveBuffer "optimize-coding" Bool  -- Compute optimal Huffman coding tables
instance HasArgument JpegsaveBuffer "interlace" Bool        -- Generate an interlaced (progressive) jpeg
instance HasArgument JpegsaveBuffer "trellis-quant" Bool    -- Apply trellis quantisation to each 8x8 block
instance HasArgument JpegsaveBuffer "overshoot-deringing" Bool  -- Apply overshooting to samples with extreme values
instance HasArgument JpegsaveBuffer "optimize-scans" Bool   -- Split spectrum of DCT coefficients into separate scans
instance HasArgument JpegsaveBuffer "quant-table" Int32     -- Use predefined quantization table with given index
instance HasArgument JpegsaveBuffer "subsample-mode" GV.ForeignJpegSubsample  -- Select chroma subsample operation mode
instance HasArgument JpegsaveBuffer "strip" Bool            -- Strip all metadata from image
instance HasArgument JpegsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument JpegsaveBuffer "page-height" Int32     -- Set page height for multipage save
instance HasOutput JpegsaveBuffer "buffer" GV.Blob          -- Buffer to save to

type Jpegsave = VipsOp "jpegsave" ()
instance HasArgument Jpegsave "in" GV.Image                 -- Image to save
instance HasArgument Jpegsave "filename" T.Text             -- Filename to save to
instance HasArgument Jpegsave "Q" Int32                     -- Q factor
instance HasArgument Jpegsave "profile" T.Text              -- ICC profile to embed
instance HasArgument Jpegsave "optimize-coding" Bool        -- Compute optimal Huffman coding tables
instance HasArgument Jpegsave "interlace" Bool              -- Generate an interlaced (progressive) jpeg
instance HasArgument Jpegsave "trellis-quant" Bool          -- Apply trellis quantisation to each 8x8 block
instance HasArgument Jpegsave "overshoot-deringing" Bool    -- Apply overshooting to samples with extreme values
instance HasArgument Jpegsave "optimize-scans" Bool         -- Split spectrum of DCT coefficients into separate scans
instance HasArgument Jpegsave "quant-table" Int32           -- Use predefined quantization table with given index
instance HasArgument Jpegsave "subsample-mode" GV.ForeignJpegSubsample  -- Select chroma subsample operation mode
instance HasArgument Jpegsave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Jpegsave "background" GV.ArrayDouble   -- Background value
instance HasArgument Jpegsave "page-height" Int32           -- Set page height for multipage save

type PngsaveTarget = VipsOp "pngsave_target" ()
instance HasArgument PngsaveTarget "in" GV.Image            -- Image to save
instance HasArgument PngsaveTarget "target" VipsTarget      -- Target to save to
instance HasArgument PngsaveTarget "compression" Int32      -- Compression factor
instance HasArgument PngsaveTarget "interlace" Bool         -- Interlace image
instance HasArgument PngsaveTarget "profile" T.Text         -- ICC profile to embed
instance HasArgument PngsaveTarget "filter" GV.ForeignPngFilter  -- libpng row filter flag(s)
instance HasArgument PngsaveTarget "palette" Bool           -- Quantise to 8bpp palette
instance HasArgument PngsaveTarget "Q" Int32                -- Quantisation quality
instance HasArgument PngsaveTarget "dither" Double          -- Amount of dithering
instance HasArgument PngsaveTarget "bitdepth" Int32         -- Write as a 1, 2, 4 or 8 bit image
instance HasArgument PngsaveTarget "strip" Bool             -- Strip all metadata from image
instance HasArgument PngsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument PngsaveTarget "page-height" Int32      -- Set page height for multipage save

type PngsaveBuffer = VipsOp "pngsave_buffer" GV.Blob
instance HasArgument PngsaveBuffer "in" GV.Image            -- Image to save
instance HasArgument PngsaveBuffer "compression" Int32      -- Compression factor
instance HasArgument PngsaveBuffer "interlace" Bool         -- Interlace image
instance HasArgument PngsaveBuffer "profile" T.Text         -- ICC profile to embed
instance HasArgument PngsaveBuffer "filter" GV.ForeignPngFilter  -- libpng row filter flag(s)
instance HasArgument PngsaveBuffer "palette" Bool           -- Quantise to 8bpp palette
instance HasArgument PngsaveBuffer "Q" Int32                -- Quantisation quality
instance HasArgument PngsaveBuffer "dither" Double          -- Amount of dithering
instance HasArgument PngsaveBuffer "bitdepth" Int32         -- Write as a 1, 2, 4 or 8 bit image
instance HasArgument PngsaveBuffer "strip" Bool             -- Strip all metadata from image
instance HasArgument PngsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument PngsaveBuffer "page-height" Int32      -- Set page height for multipage save
instance HasOutput PngsaveBuffer "buffer" GV.Blob           -- Buffer to save to

type Pngsave = VipsOp "pngsave" ()
instance HasArgument Pngsave "in" GV.Image                  -- Image to save
instance HasArgument Pngsave "filename" T.Text              -- Filename to save to
instance HasArgument Pngsave "compression" Int32            -- Compression factor
instance HasArgument Pngsave "interlace" Bool               -- Interlace image
instance HasArgument Pngsave "profile" T.Text               -- ICC profile to embed
instance HasArgument Pngsave "filter" GV.ForeignPngFilter   -- libpng row filter flag(s)
instance HasArgument Pngsave "palette" Bool                 -- Quantise to 8bpp palette
instance HasArgument Pngsave "Q" Int32                      -- Quantisation quality
instance HasArgument Pngsave "dither" Double                -- Amount of dithering
instance HasArgument Pngsave "bitdepth" Int32               -- Write as a 1, 2, 4 or 8 bit image
instance HasArgument Pngsave "strip" Bool                   -- Strip all metadata from image
instance HasArgument Pngsave "background" GV.ArrayDouble    -- Background value
instance HasArgument Pngsave "page-height" Int32            -- Set page height for multipage save

type DzsaveBuffer = VipsOp "dzsave_buffer" GV.Blob
instance HasArgument DzsaveBuffer "in" GV.Image             -- Image to save
instance HasArgument DzsaveBuffer "basename" T.Text         -- Base name to save to
instance HasArgument DzsaveBuffer "layout" GV.ForeignDzLayout  -- Directory layout
instance HasArgument DzsaveBuffer "suffix" T.Text           -- Filename suffix for tiles
instance HasArgument DzsaveBuffer "overlap" Int32           -- Tile overlap in pixels
instance HasArgument DzsaveBuffer "tile-size" Int32         -- Tile size in pixels
instance HasArgument DzsaveBuffer "depth" GV.ForeignDzDepth  -- Pyramid depth
instance HasArgument DzsaveBuffer "centre" Bool             -- Center image in tile
instance HasArgument DzsaveBuffer "angle" GV.Angle          -- Rotate image during save
instance HasArgument DzsaveBuffer "container" GV.ForeignDzContainer  -- Pyramid container type
instance HasArgument DzsaveBuffer "properties" Bool         -- Write a properties file to the output directory
instance HasArgument DzsaveBuffer "compression" Int32       -- ZIP deflate compression level
instance HasArgument DzsaveBuffer "region-shrink" GV.RegionShrink  -- Method to shrink regions
instance HasArgument DzsaveBuffer "skip-blanks" Int32       -- Skip tiles which are nearly equal to the background
instance HasArgument DzsaveBuffer "no-strip" Bool           -- Don't strip tile metadata
instance HasArgument DzsaveBuffer "id" T.Text               -- Resource ID
instance HasArgument DzsaveBuffer "strip" Bool              -- Strip all metadata from image
instance HasArgument DzsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument DzsaveBuffer "page-height" Int32       -- Set page height for multipage save
instance HasOutput DzsaveBuffer "buffer" GV.Blob            -- Buffer to save to

type Dzsave = VipsOp "dzsave" ()
instance HasArgument Dzsave "in" GV.Image                   -- Image to save
instance HasArgument Dzsave "filename" T.Text               -- Filename to save to
instance HasArgument Dzsave "basename" T.Text               -- Base name to save to
instance HasArgument Dzsave "layout" GV.ForeignDzLayout     -- Directory layout
instance HasArgument Dzsave "suffix" T.Text                 -- Filename suffix for tiles
instance HasArgument Dzsave "overlap" Int32                 -- Tile overlap in pixels
instance HasArgument Dzsave "tile-size" Int32               -- Tile size in pixels
instance HasArgument Dzsave "depth" GV.ForeignDzDepth       -- Pyramid depth
instance HasArgument Dzsave "centre" Bool                   -- Center image in tile
instance HasArgument Dzsave "angle" GV.Angle                -- Rotate image during save
instance HasArgument Dzsave "container" GV.ForeignDzContainer  -- Pyramid container type
instance HasArgument Dzsave "properties" Bool               -- Write a properties file to the output directory
instance HasArgument Dzsave "compression" Int32             -- ZIP deflate compression level
instance HasArgument Dzsave "region-shrink" GV.RegionShrink  -- Method to shrink regions
instance HasArgument Dzsave "skip-blanks" Int32             -- Skip tiles which are nearly equal to the background
instance HasArgument Dzsave "no-strip" Bool                 -- Don't strip tile metadata
instance HasArgument Dzsave "id" T.Text                     -- Resource ID
instance HasArgument Dzsave "strip" Bool                    -- Strip all metadata from image
instance HasArgument Dzsave "background" GV.ArrayDouble     -- Background value
instance HasArgument Dzsave "page-height" Int32             -- Set page height for multipage save

type RadsaveTarget = VipsOp "radsave_target" ()
instance HasArgument RadsaveTarget "in" GV.Image            -- Image to save
instance HasArgument RadsaveTarget "target" VipsTarget      -- Target to save to
instance HasArgument RadsaveTarget "strip" Bool             -- Strip all metadata from image
instance HasArgument RadsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument RadsaveTarget "page-height" Int32      -- Set page height for multipage save

type RadsaveBuffer = VipsOp "radsave_buffer" GV.Blob
instance HasArgument RadsaveBuffer "in" GV.Image            -- Image to save
instance HasArgument RadsaveBuffer "strip" Bool             -- Strip all metadata from image
instance HasArgument RadsaveBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument RadsaveBuffer "page-height" Int32      -- Set page height for multipage save
instance HasOutput RadsaveBuffer "buffer" GV.Blob           -- Buffer to save to

type Radsave = VipsOp "radsave" ()
instance HasArgument Radsave "in" GV.Image                  -- Image to save
instance HasArgument Radsave "filename" T.Text              -- Filename to save to
instance HasArgument Radsave "strip" Bool                   -- Strip all metadata from image
instance HasArgument Radsave "background" GV.ArrayDouble    -- Background value
instance HasArgument Radsave "page-height" Int32            -- Set page height for multipage save

type PpmsaveTarget = VipsOp "ppmsave_target" ()
instance HasArgument PpmsaveTarget "in" GV.Image            -- Image to save
instance HasArgument PpmsaveTarget "target" VipsTarget      -- Target to save to
instance HasArgument PpmsaveTarget "ascii" Bool             -- save as ascii
instance HasArgument PpmsaveTarget "bitdepth" Int32         -- set to 1 to write as a 1 bit image
instance HasArgument PpmsaveTarget "strip" Bool             -- Strip all metadata from image
instance HasArgument PpmsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument PpmsaveTarget "page-height" Int32      -- Set page height for multipage save

type Ppmsave = VipsOp "ppmsave" ()
instance HasArgument Ppmsave "in" GV.Image                  -- Image to save
instance HasArgument Ppmsave "filename" T.Text              -- Filename to save to
instance HasArgument Ppmsave "ascii" Bool                   -- save as ascii
instance HasArgument Ppmsave "bitdepth" Int32               -- set to 1 to write as a 1 bit image
instance HasArgument Ppmsave "strip" Bool                   -- Strip all metadata from image
instance HasArgument Ppmsave "background" GV.ArrayDouble    -- Background value
instance HasArgument Ppmsave "page-height" Int32            -- Set page height for multipage save

type Vipssave = VipsOp "vipssave" ()
instance HasArgument Vipssave "in" GV.Image                 -- Image to save
instance HasArgument Vipssave "filename" T.Text             -- Filename to save to
instance HasArgument Vipssave "strip" Bool                  -- Strip all metadata from image
instance HasArgument Vipssave "background" GV.ArrayDouble   -- Background value
instance HasArgument Vipssave "page-height" Int32           -- Set page height for multipage save

type RawsaveFd = VipsOp "rawsave_fd" ()
instance HasArgument RawsaveFd "in" GV.Image                -- Image to save
instance HasArgument RawsaveFd "fd" Int32                   -- File descriptor to write to
instance HasArgument RawsaveFd "strip" Bool                 -- Strip all metadata from image
instance HasArgument RawsaveFd "background" GV.ArrayDouble  -- Background value
instance HasArgument RawsaveFd "page-height" Int32          -- Set page height for multipage save

type Rawsave = VipsOp "rawsave" ()
instance HasArgument Rawsave "in" GV.Image                  -- Image to save
instance HasArgument Rawsave "filename" T.Text              -- Filename to save to
instance HasArgument Rawsave "strip" Bool                   -- Strip all metadata from image
instance HasArgument Rawsave "background" GV.ArrayDouble    -- Background value
instance HasArgument Rawsave "page-height" Int32            -- Set page height for multipage save

type Matrixprint = VipsOp "matrixprint" ()
instance HasArgument Matrixprint "in" GV.Image              -- Image to save
instance HasArgument Matrixprint "strip" Bool               -- Strip all metadata from image
instance HasArgument Matrixprint "background" GV.ArrayDouble  -- Background value
instance HasArgument Matrixprint "page-height" Int32        -- Set page height for multipage save

type MatrixsaveTarget = VipsOp "matrixsave_target" ()
instance HasArgument MatrixsaveTarget "in" GV.Image         -- Image to save
instance HasArgument MatrixsaveTarget "target" VipsTarget   -- Target to save to
instance HasArgument MatrixsaveTarget "strip" Bool          -- Strip all metadata from image
instance HasArgument MatrixsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument MatrixsaveTarget "page-height" Int32   -- Set page height for multipage save

type Matrixsave = VipsOp "matrixsave" ()
instance HasArgument Matrixsave "in" GV.Image               -- Image to save
instance HasArgument Matrixsave "filename" T.Text           -- Filename to save to
instance HasArgument Matrixsave "strip" Bool                -- Strip all metadata from image
instance HasArgument Matrixsave "background" GV.ArrayDouble  -- Background value
instance HasArgument Matrixsave "page-height" Int32         -- Set page height for multipage save

type CsvsaveTarget = VipsOp "csvsave_target" ()
instance HasArgument CsvsaveTarget "in" GV.Image            -- Image to save
instance HasArgument CsvsaveTarget "target" VipsTarget      -- Target to save to
instance HasArgument CsvsaveTarget "separator" T.Text       -- Separator characters
instance HasArgument CsvsaveTarget "strip" Bool             -- Strip all metadata from image
instance HasArgument CsvsaveTarget "background" GV.ArrayDouble  -- Background value
instance HasArgument CsvsaveTarget "page-height" Int32      -- Set page height for multipage save

type Csvsave = VipsOp "csvsave" ()
instance HasArgument Csvsave "in" GV.Image                  -- Image to save
instance HasArgument Csvsave "filename" T.Text              -- Filename to save to
instance HasArgument Csvsave "separator" T.Text             -- Separator characters
instance HasArgument Csvsave "strip" Bool                   -- Strip all metadata from image
instance HasArgument Csvsave "background" GV.ArrayDouble    -- Background value
instance HasArgument Csvsave "page-height" Int32            -- Set page height for multipage save

type HeifloadSource = VipsOp "heifload_source" ImgLoadResult
instance HasArgument HeifloadSource "source" VipsSource     -- Source to load from
instance HasArgument HeifloadSource "page" Int32            -- Load this page from the file
instance HasArgument HeifloadSource "n" Int32               -- Load this many pages
instance HasArgument HeifloadSource "thumbnail" Bool        -- Fetch thumbnail image
instance HasArgument HeifloadSource "memory" Bool           -- Force open via memory
instance HasArgument HeifloadSource "access" GV.Access      -- Required access pattern for this file
instance HasArgument HeifloadSource "fail" Bool             -- Fail on first error
instance HasOutput HeifloadSource "out" GV.Image            -- Output image
instance HasOutput HeifloadSource "flags" GV.ForeignFlags   -- Flags for this file

type HeifloadBuffer = VipsOp "heifload_buffer" ImgLoadResult
instance HasArgument HeifloadBuffer "buffer" GV.Blob        -- Buffer to load from
instance HasArgument HeifloadBuffer "page" Int32            -- Load this page from the file
instance HasArgument HeifloadBuffer "n" Int32               -- Load this many pages
instance HasArgument HeifloadBuffer "thumbnail" Bool        -- Fetch thumbnail image
instance HasArgument HeifloadBuffer "memory" Bool           -- Force open via memory
instance HasArgument HeifloadBuffer "access" GV.Access      -- Required access pattern for this file
instance HasArgument HeifloadBuffer "fail" Bool             -- Fail on first error
instance HasOutput HeifloadBuffer "out" GV.Image            -- Output image
instance HasOutput HeifloadBuffer "flags" GV.ForeignFlags   -- Flags for this file

type Heifload = VipsOp "heifload" ImgLoadResult
instance HasArgument Heifload "filename" T.Text             -- Filename to load from
instance HasArgument Heifload "page" Int32                  -- Load this page from the file
instance HasArgument Heifload "n" Int32                     -- Load this many pages
instance HasArgument Heifload "thumbnail" Bool              -- Fetch thumbnail image
instance HasArgument Heifload "memory" Bool                 -- Force open via memory
instance HasArgument Heifload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Heifload "fail" Bool                   -- Fail on first error
instance HasOutput Heifload "out" GV.Image                  -- Output image
instance HasOutput Heifload "flags" GV.ForeignFlags         -- Flags for this file

type Openexrload = VipsOp "openexrload" ImgLoadResult
instance HasArgument Openexrload "filename" T.Text          -- Filename to load from
instance HasArgument Openexrload "memory" Bool              -- Force open via memory
instance HasArgument Openexrload "access" GV.Access         -- Required access pattern for this file
instance HasArgument Openexrload "fail" Bool                -- Fail on first error
instance HasOutput Openexrload "out" GV.Image               -- Output image
instance HasOutput Openexrload "flags" GV.ForeignFlags      -- Flags for this file

type Fitsload = VipsOp "fitsload" ImgLoadResult
instance HasArgument Fitsload "filename" T.Text             -- Filename to load from
instance HasArgument Fitsload "memory" Bool                 -- Force open via memory
instance HasArgument Fitsload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Fitsload "fail" Bool                   -- Fail on first error
instance HasOutput Fitsload "out" GV.Image                  -- Output image
instance HasOutput Fitsload "flags" GV.ForeignFlags         -- Flags for this file

type MagickloadBuffer = VipsOp "magickload_buffer" ImgLoadResult
instance HasArgument MagickloadBuffer "buffer" GV.Blob      -- Buffer to load from
instance HasArgument MagickloadBuffer "density" T.Text      -- Canvas resolution for rendering vector formats like SVG
instance HasArgument MagickloadBuffer "page" Int32          -- Load this page from the file
instance HasArgument MagickloadBuffer "n" Int32             -- Load this many pages
instance HasArgument MagickloadBuffer "memory" Bool         -- Force open via memory
instance HasArgument MagickloadBuffer "access" GV.Access    -- Required access pattern for this file
instance HasArgument MagickloadBuffer "fail" Bool           -- Fail on first error
instance HasOutput MagickloadBuffer "out" GV.Image          -- Output image
instance HasOutput MagickloadBuffer "flags" GV.ForeignFlags  -- Flags for this file

type Magickload = VipsOp "magickload" ImgLoadResult
instance HasArgument Magickload "filename" T.Text           -- Filename to load from
instance HasArgument Magickload "density" T.Text            -- Canvas resolution for rendering vector formats like SVG
instance HasArgument Magickload "page" Int32                -- Load this page from the file
instance HasArgument Magickload "n" Int32                   -- Load this many pages
instance HasArgument Magickload "memory" Bool               -- Force open via memory
instance HasArgument Magickload "access" GV.Access          -- Required access pattern for this file
instance HasArgument Magickload "fail" Bool                 -- Fail on first error
instance HasOutput Magickload "out" GV.Image                -- Output image
instance HasOutput Magickload "flags" GV.ForeignFlags       -- Flags for this file

type TiffloadSource = VipsOp "tiffload_source" ImgLoadResult
instance HasArgument TiffloadSource "source" VipsSource     -- Source to load from
instance HasArgument TiffloadSource "page" Int32            -- Load this page from the image
instance HasArgument TiffloadSource "n" Int32               -- Load this many pages
instance HasArgument TiffloadSource "subifd" Int32          -- Select subifd index
instance HasArgument TiffloadSource "autorotate" Bool       -- Rotate image using orientation tag
instance HasArgument TiffloadSource "memory" Bool           -- Force open via memory
instance HasArgument TiffloadSource "access" GV.Access      -- Required access pattern for this file
instance HasArgument TiffloadSource "fail" Bool             -- Fail on first error
instance HasOutput TiffloadSource "out" GV.Image            -- Output image
instance HasOutput TiffloadSource "flags" GV.ForeignFlags   -- Flags for this file

type TiffloadBuffer = VipsOp "tiffload_buffer" ImgLoadResult
instance HasArgument TiffloadBuffer "buffer" GV.Blob        -- Buffer to load from
instance HasArgument TiffloadBuffer "page" Int32            -- Load this page from the image
instance HasArgument TiffloadBuffer "n" Int32               -- Load this many pages
instance HasArgument TiffloadBuffer "subifd" Int32          -- Select subifd index
instance HasArgument TiffloadBuffer "autorotate" Bool       -- Rotate image using orientation tag
instance HasArgument TiffloadBuffer "memory" Bool           -- Force open via memory
instance HasArgument TiffloadBuffer "access" GV.Access      -- Required access pattern for this file
instance HasArgument TiffloadBuffer "fail" Bool             -- Fail on first error
instance HasOutput TiffloadBuffer "out" GV.Image            -- Output image
instance HasOutput TiffloadBuffer "flags" GV.ForeignFlags   -- Flags for this file

type Tiffload = VipsOp "tiffload" ImgLoadResult
instance HasArgument Tiffload "filename" T.Text             -- Filename to load from
instance HasArgument Tiffload "page" Int32                  -- Load this page from the image
instance HasArgument Tiffload "n" Int32                     -- Load this many pages
instance HasArgument Tiffload "subifd" Int32                -- Select subifd index
instance HasArgument Tiffload "autorotate" Bool             -- Rotate image using orientation tag
instance HasArgument Tiffload "memory" Bool                 -- Force open via memory
instance HasArgument Tiffload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Tiffload "fail" Bool                   -- Fail on first error
instance HasOutput Tiffload "out" GV.Image                  -- Output image
instance HasOutput Tiffload "flags" GV.ForeignFlags         -- Flags for this file

type WebploadSource = VipsOp "webpload_source" ImgLoadResult
instance HasArgument WebploadSource "source" VipsSource     -- Source to load from
instance HasArgument WebploadSource "page" Int32            -- Load this page from the file
instance HasArgument WebploadSource "n" Int32               -- Load this many pages
instance HasArgument WebploadSource "scale" Double          -- Scale factor on load
instance HasArgument WebploadSource "memory" Bool           -- Force open via memory
instance HasArgument WebploadSource "access" GV.Access      -- Required access pattern for this file
instance HasArgument WebploadSource "fail" Bool             -- Fail on first error
instance HasOutput WebploadSource "out" GV.Image            -- Output image
instance HasOutput WebploadSource "flags" GV.ForeignFlags   -- Flags for this file

type WebploadBuffer = VipsOp "webpload_buffer" ImgLoadResult
instance HasArgument WebploadBuffer "buffer" GV.Blob        -- Buffer to load from
instance HasArgument WebploadBuffer "page" Int32            -- Load this page from the file
instance HasArgument WebploadBuffer "n" Int32               -- Load this many pages
instance HasArgument WebploadBuffer "scale" Double          -- Scale factor on load
instance HasArgument WebploadBuffer "memory" Bool           -- Force open via memory
instance HasArgument WebploadBuffer "access" GV.Access      -- Required access pattern for this file
instance HasArgument WebploadBuffer "fail" Bool             -- Fail on first error
instance HasOutput WebploadBuffer "out" GV.Image            -- Output image
instance HasOutput WebploadBuffer "flags" GV.ForeignFlags   -- Flags for this file

type Webpload = VipsOp "webpload" ImgLoadResult
instance HasArgument Webpload "filename" T.Text             -- Filename to load from
instance HasArgument Webpload "page" Int32                  -- Load this page from the file
instance HasArgument Webpload "n" Int32                     -- Load this many pages
instance HasArgument Webpload "scale" Double                -- Scale factor on load
instance HasArgument Webpload "memory" Bool                 -- Force open via memory
instance HasArgument Webpload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Webpload "fail" Bool                   -- Fail on first error
instance HasOutput Webpload "out" GV.Image                  -- Output image
instance HasOutput Webpload "flags" GV.ForeignFlags         -- Flags for this file

type JpegloadSource = VipsOp "jpegload_source" ImgLoadResult
instance HasArgument JpegloadSource "source" VipsSource     -- Source to load from
instance HasArgument JpegloadSource "shrink" Int32          -- Shrink factor on load
instance HasArgument JpegloadSource "autorotate" Bool       -- Rotate image using exif orientation
instance HasArgument JpegloadSource "memory" Bool           -- Force open via memory
instance HasArgument JpegloadSource "access" GV.Access      -- Required access pattern for this file
instance HasArgument JpegloadSource "fail" Bool             -- Fail on first error
instance HasOutput JpegloadSource "out" GV.Image            -- Output image
instance HasOutput JpegloadSource "flags" GV.ForeignFlags   -- Flags for this file

type JpegloadBuffer = VipsOp "jpegload_buffer" ImgLoadResult
instance HasArgument JpegloadBuffer "buffer" GV.Blob        -- Buffer to load from
instance HasArgument JpegloadBuffer "shrink" Int32          -- Shrink factor on load
instance HasArgument JpegloadBuffer "autorotate" Bool       -- Rotate image using exif orientation
instance HasArgument JpegloadBuffer "memory" Bool           -- Force open via memory
instance HasArgument JpegloadBuffer "access" GV.Access      -- Required access pattern for this file
instance HasArgument JpegloadBuffer "fail" Bool             -- Fail on first error
instance HasOutput JpegloadBuffer "out" GV.Image            -- Output image
instance HasOutput JpegloadBuffer "flags" GV.ForeignFlags   -- Flags for this file

type Jpegload = VipsOp "jpegload" ImgLoadResult
instance HasArgument Jpegload "filename" T.Text             -- Filename to load from
instance HasArgument Jpegload "shrink" Int32                -- Shrink factor on load
instance HasArgument Jpegload "autorotate" Bool             -- Rotate image using exif orientation
instance HasArgument Jpegload "memory" Bool                 -- Force open via memory
instance HasArgument Jpegload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Jpegload "fail" Bool                   -- Fail on first error
instance HasOutput Jpegload "out" GV.Image                  -- Output image
instance HasOutput Jpegload "flags" GV.ForeignFlags         -- Flags for this file

type PngloadSource = VipsOp "pngload_source" ImgLoadResult
instance HasArgument PngloadSource "source" VipsSource      -- Source to load from
instance HasArgument PngloadSource "memory" Bool            -- Force open via memory
instance HasArgument PngloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument PngloadSource "fail" Bool              -- Fail on first error
instance HasOutput PngloadSource "out" GV.Image             -- Output image
instance HasOutput PngloadSource "flags" GV.ForeignFlags    -- Flags for this file

type PngloadBuffer = VipsOp "pngload_buffer" ImgLoadResult
instance HasArgument PngloadBuffer "buffer" GV.Blob         -- Buffer to load from
instance HasArgument PngloadBuffer "memory" Bool            -- Force open via memory
instance HasArgument PngloadBuffer "access" GV.Access       -- Required access pattern for this file
instance HasArgument PngloadBuffer "fail" Bool              -- Fail on first error
instance HasOutput PngloadBuffer "out" GV.Image             -- Output image
instance HasOutput PngloadBuffer "flags" GV.ForeignFlags    -- Flags for this file

type Pngload = VipsOp "pngload" ImgLoadResult
instance HasArgument Pngload "filename" T.Text              -- Filename to load from
instance HasArgument Pngload "memory" Bool                  -- Force open via memory
instance HasArgument Pngload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Pngload "fail" Bool                    -- Fail on first error
instance HasOutput Pngload "out" GV.Image                   -- Output image
instance HasOutput Pngload "flags" GV.ForeignFlags          -- Flags for this file

type GifloadSource = VipsOp "gifload_source" ImgLoadResult
instance HasArgument GifloadSource "source" VipsSource      -- Source to load from
instance HasArgument GifloadSource "page" Int32             -- Load this page from the file
instance HasArgument GifloadSource "n" Int32                -- Load this many pages
instance HasArgument GifloadSource "memory" Bool            -- Force open via memory
instance HasArgument GifloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument GifloadSource "fail" Bool              -- Fail on first error
instance HasOutput GifloadSource "out" GV.Image             -- Output image
instance HasOutput GifloadSource "flags" GV.ForeignFlags    -- Flags for this file

type GifloadBuffer = VipsOp "gifload_buffer" ImgLoadResult
instance HasArgument GifloadBuffer "buffer" GV.Blob         -- Buffer to load from
instance HasArgument GifloadBuffer "page" Int32             -- Load this page from the file
instance HasArgument GifloadBuffer "n" Int32                -- Load this many pages
instance HasArgument GifloadBuffer "memory" Bool            -- Force open via memory
instance HasArgument GifloadBuffer "access" GV.Access       -- Required access pattern for this file
instance HasArgument GifloadBuffer "fail" Bool              -- Fail on first error
instance HasOutput GifloadBuffer "out" GV.Image             -- Output image
instance HasOutput GifloadBuffer "flags" GV.ForeignFlags    -- Flags for this file

type Gifload = VipsOp "gifload" ImgLoadResult
instance HasArgument Gifload "filename" T.Text              -- Filename to load from
instance HasArgument Gifload "page" Int32                   -- Load this page from the file
instance HasArgument Gifload "n" Int32                      -- Load this many pages
instance HasArgument Gifload "memory" Bool                  -- Force open via memory
instance HasArgument Gifload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Gifload "fail" Bool                    -- Fail on first error
instance HasOutput Gifload "out" GV.Image                   -- Output image
instance HasOutput Gifload "flags" GV.ForeignFlags          -- Flags for this file

type SvgloadSource = VipsOp "svgload_source" ImgLoadResult
instance HasArgument SvgloadSource "source" VipsSource      -- Source to load from
instance HasArgument SvgloadSource "dpi" Double             -- Render at this DPI
instance HasArgument SvgloadSource "scale" Double           -- Scale output by this factor
instance HasArgument SvgloadSource "unlimited" Bool         -- Allow SVG of any size
instance HasArgument SvgloadSource "memory" Bool            -- Force open via memory
instance HasArgument SvgloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument SvgloadSource "fail" Bool              -- Fail on first error
instance HasOutput SvgloadSource "out" GV.Image             -- Output image
instance HasOutput SvgloadSource "flags" GV.ForeignFlags    -- Flags for this file

type SvgloadBuffer = VipsOp "svgload_buffer" ImgLoadResult
instance HasArgument SvgloadBuffer "buffer" GV.Blob         -- Buffer to load from
instance HasArgument SvgloadBuffer "dpi" Double             -- Render at this DPI
instance HasArgument SvgloadBuffer "scale" Double           -- Scale output by this factor
instance HasArgument SvgloadBuffer "unlimited" Bool         -- Allow SVG of any size
instance HasArgument SvgloadBuffer "memory" Bool            -- Force open via memory
instance HasArgument SvgloadBuffer "access" GV.Access       -- Required access pattern for this file
instance HasArgument SvgloadBuffer "fail" Bool              -- Fail on first error
instance HasOutput SvgloadBuffer "out" GV.Image             -- Output image
instance HasOutput SvgloadBuffer "flags" GV.ForeignFlags    -- Flags for this file

type Svgload = VipsOp "svgload" ImgLoadResult
instance HasArgument Svgload "filename" T.Text              -- Filename to load from
instance HasArgument Svgload "dpi" Double                   -- Render at this DPI
instance HasArgument Svgload "scale" Double                 -- Scale output by this factor
instance HasArgument Svgload "unlimited" Bool               -- Allow SVG of any size
instance HasArgument Svgload "memory" Bool                  -- Force open via memory
instance HasArgument Svgload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Svgload "fail" Bool                    -- Fail on first error
instance HasOutput Svgload "out" GV.Image                   -- Output image
instance HasOutput Svgload "flags" GV.ForeignFlags          -- Flags for this file

type PdfloadSource = VipsOp "pdfload_source" ImgLoadResult
instance HasArgument PdfloadSource "source" VipsSource      -- Source to load from
instance HasArgument PdfloadSource "page" Int32             -- Load this page from the file
instance HasArgument PdfloadSource "n" Int32                -- Load this many pages
instance HasArgument PdfloadSource "dpi" Double             -- Render at this DPI
instance HasArgument PdfloadSource "scale" Double           -- Scale output by this factor
instance HasArgument PdfloadSource "background" GV.ArrayDouble  -- Background value
instance HasArgument PdfloadSource "memory" Bool            -- Force open via memory
instance HasArgument PdfloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument PdfloadSource "fail" Bool              -- Fail on first error
instance HasOutput PdfloadSource "out" GV.Image             -- Output image
instance HasOutput PdfloadSource "flags" GV.ForeignFlags    -- Flags for this file

type PdfloadBuffer = VipsOp "pdfload_buffer" ImgLoadResult
instance HasArgument PdfloadBuffer "buffer" GV.Blob         -- Buffer to load from
instance HasArgument PdfloadBuffer "page" Int32             -- Load this page from the file
instance HasArgument PdfloadBuffer "n" Int32                -- Load this many pages
instance HasArgument PdfloadBuffer "dpi" Double             -- Render at this DPI
instance HasArgument PdfloadBuffer "scale" Double           -- Scale output by this factor
instance HasArgument PdfloadBuffer "background" GV.ArrayDouble  -- Background value
instance HasArgument PdfloadBuffer "memory" Bool            -- Force open via memory
instance HasArgument PdfloadBuffer "access" GV.Access       -- Required access pattern for this file
instance HasArgument PdfloadBuffer "fail" Bool              -- Fail on first error
instance HasOutput PdfloadBuffer "out" GV.Image             -- Output image
instance HasOutput PdfloadBuffer "flags" GV.ForeignFlags    -- Flags for this file

type Pdfload = VipsOp "pdfload" ImgLoadResult
instance HasArgument Pdfload "filename" T.Text              -- Filename to load from
instance HasArgument Pdfload "page" Int32                   -- Load this page from the file
instance HasArgument Pdfload "n" Int32                      -- Load this many pages
instance HasArgument Pdfload "dpi" Double                   -- Render at this DPI
instance HasArgument Pdfload "scale" Double                 -- Scale output by this factor
instance HasArgument Pdfload "background" GV.ArrayDouble    -- Background value
instance HasArgument Pdfload "memory" Bool                  -- Force open via memory
instance HasArgument Pdfload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Pdfload "fail" Bool                    -- Fail on first error
instance HasOutput Pdfload "out" GV.Image                   -- Output image
instance HasOutput Pdfload "flags" GV.ForeignFlags          -- Flags for this file

type RadloadSource = VipsOp "radload_source" ImgLoadResult
instance HasArgument RadloadSource "source" VipsSource      -- Source to load from
instance HasArgument RadloadSource "memory" Bool            -- Force open via memory
instance HasArgument RadloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument RadloadSource "fail" Bool              -- Fail on first error
instance HasOutput RadloadSource "out" GV.Image             -- Output image
instance HasOutput RadloadSource "flags" GV.ForeignFlags    -- Flags for this file

type RadloadBuffer = VipsOp "radload_buffer" ImgLoadResult
instance HasArgument RadloadBuffer "buffer" GV.Blob         -- Buffer to load from
instance HasArgument RadloadBuffer "memory" Bool            -- Force open via memory
instance HasArgument RadloadBuffer "access" GV.Access       -- Required access pattern for this file
instance HasArgument RadloadBuffer "fail" Bool              -- Fail on first error
instance HasOutput RadloadBuffer "out" GV.Image             -- Output image
instance HasOutput RadloadBuffer "flags" GV.ForeignFlags    -- Flags for this file

type Radload = VipsOp "radload" ImgLoadResult
instance HasArgument Radload "filename" T.Text              -- Filename to load from
instance HasArgument Radload "memory" Bool                  -- Force open via memory
instance HasArgument Radload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Radload "fail" Bool                    -- Fail on first error
instance HasOutput Radload "out" GV.Image                   -- Output image
instance HasOutput Radload "flags" GV.ForeignFlags          -- Flags for this file

type PpmloadSource = VipsOp "ppmload_source" ImgLoadResult
instance HasArgument PpmloadSource "source" VipsSource      -- Source to load from
instance HasArgument PpmloadSource "memory" Bool            -- Force open via memory
instance HasArgument PpmloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument PpmloadSource "fail" Bool              -- Fail on first error
instance HasOutput PpmloadSource "out" GV.Image             -- Output image
instance HasOutput PpmloadSource "flags" GV.ForeignFlags    -- Flags for this file

type Ppmload = VipsOp "ppmload" ImgLoadResult
instance HasArgument Ppmload "filename" T.Text              -- Filename to load from
instance HasArgument Ppmload "memory" Bool                  -- Force open via memory
instance HasArgument Ppmload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Ppmload "fail" Bool                    -- Fail on first error
instance HasOutput Ppmload "out" GV.Image                   -- Output image
instance HasOutput Ppmload "flags" GV.ForeignFlags          -- Flags for this file

type Analyzeload = VipsOp "analyzeload" ImgLoadResult
instance HasArgument Analyzeload "filename" T.Text          -- Filename to load from
instance HasArgument Analyzeload "memory" Bool              -- Force open via memory
instance HasArgument Analyzeload "access" GV.Access         -- Required access pattern for this file
instance HasArgument Analyzeload "fail" Bool                -- Fail on first error
instance HasOutput Analyzeload "out" GV.Image               -- Output image
instance HasOutput Analyzeload "flags" GV.ForeignFlags      -- Flags for this file

type Vipsload = VipsOp "vipsload" ImgLoadResult
instance HasArgument Vipsload "filename" T.Text             -- Filename to load from
instance HasArgument Vipsload "memory" Bool                 -- Force open via memory
instance HasArgument Vipsload "access" GV.Access            -- Required access pattern for this file
instance HasArgument Vipsload "fail" Bool                   -- Fail on first error
instance HasOutput Vipsload "out" GV.Image                  -- Output image
instance HasOutput Vipsload "flags" GV.ForeignFlags         -- Flags for this file

type Rawload = VipsOp "rawload" ImgLoadResult
instance HasArgument Rawload "filename" T.Text              -- Filename to load from
instance HasArgument Rawload "width" Int32                  -- Image width in pixels
instance HasArgument Rawload "height" Int32                 -- Image height in pixels
instance HasArgument Rawload "bands" Int32                  -- Number of bands in image
instance HasArgument Rawload "offset" Word64                -- Offset in bytes from start of file
instance HasArgument Rawload "format" GV.BandFormat         -- Pixel format in image
instance HasArgument Rawload "interpretation" GV.Interpretation  -- Pixel interpretation
instance HasArgument Rawload "memory" Bool                  -- Force open via memory
instance HasArgument Rawload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Rawload "fail" Bool                    -- Fail on first error
instance HasOutput Rawload "out" GV.Image                   -- Output image
instance HasOutput Rawload "flags" GV.ForeignFlags          -- Flags for this file

type MatrixloadSource = VipsOp "matrixload_source" ImgLoadResult
instance HasArgument MatrixloadSource "source" VipsSource   -- Source to load from
instance HasArgument MatrixloadSource "memory" Bool         -- Force open via memory
instance HasArgument MatrixloadSource "access" GV.Access    -- Required access pattern for this file
instance HasArgument MatrixloadSource "fail" Bool           -- Fail on first error
instance HasOutput MatrixloadSource "out" GV.Image          -- Output image
instance HasOutput MatrixloadSource "flags" GV.ForeignFlags  -- Flags for this file

type Matrixload = VipsOp "matrixload" ImgLoadResult
instance HasArgument Matrixload "filename" T.Text           -- Filename to load from
instance HasArgument Matrixload "memory" Bool               -- Force open via memory
instance HasArgument Matrixload "access" GV.Access          -- Required access pattern for this file
instance HasArgument Matrixload "fail" Bool                 -- Fail on first error
instance HasOutput Matrixload "out" GV.Image                -- Output image
instance HasOutput Matrixload "flags" GV.ForeignFlags       -- Flags for this file

type CsvloadSource = VipsOp "csvload_source" ImgLoadResult
instance HasArgument CsvloadSource "source" VipsSource      -- Source to load from
instance HasArgument CsvloadSource "skip" Int32             -- Skip this many lines at the start of the file
instance HasArgument CsvloadSource "lines" Int32            -- Read this many lines from the file
instance HasArgument CsvloadSource "whitespace" T.Text      -- Set of whitespace characters
instance HasArgument CsvloadSource "separator" T.Text       -- Set of separator characters
instance HasArgument CsvloadSource "memory" Bool            -- Force open via memory
instance HasArgument CsvloadSource "access" GV.Access       -- Required access pattern for this file
instance HasArgument CsvloadSource "fail" Bool              -- Fail on first error
instance HasOutput CsvloadSource "out" GV.Image             -- Output image
instance HasOutput CsvloadSource "flags" GV.ForeignFlags    -- Flags for this file

type Csvload = VipsOp "csvload" ImgLoadResult
instance HasArgument Csvload "filename" T.Text              -- Filename to load from
instance HasArgument Csvload "skip" Int32                   -- Skip this many lines at the start of the file
instance HasArgument Csvload "lines" Int32                  -- Read this many lines from the file
instance HasArgument Csvload "whitespace" T.Text            -- Set of whitespace characters
instance HasArgument Csvload "separator" T.Text             -- Set of separator characters
instance HasArgument Csvload "memory" Bool                  -- Force open via memory
instance HasArgument Csvload "access" GV.Access             -- Required access pattern for this file
instance HasArgument Csvload "fail" Bool                    -- Fail on first error
instance HasOutput Csvload "out" GV.Image                   -- Output image
instance HasOutput Csvload "flags" GV.ForeignFlags          -- Flags for this file

type Switch = VipsOp "switch" GV.Image
instance HasArgument Switch "tests" GV.ArrayImage           -- Table of images to test
instance HasOutput Switch "out" GV.Image                    -- Output image

type Perlin = VipsOp "perlin" GV.Image
instance HasArgument Perlin "width" Int32                   -- Image width in pixels
instance HasArgument Perlin "height" Int32                  -- Image height in pixels
instance HasArgument Perlin "cell-size" Int32               -- Size of Perlin cells
instance HasArgument Perlin "uchar" Bool                    -- Output an unsigned char image
instance HasOutput Perlin "out" GV.Image                    -- Output image

type Worley = VipsOp "worley" GV.Image
instance HasArgument Worley "width" Int32                   -- Image width in pixels
instance HasArgument Worley "height" Int32                  -- Image height in pixels
instance HasArgument Worley "cell-size" Int32               -- Size of Worley cells
instance HasOutput Worley "out" GV.Image                    -- Output image

type Fractsurf = VipsOp "fractsurf" GV.Image
instance HasArgument Fractsurf "width" Int32                -- Image width in pixels
instance HasArgument Fractsurf "height" Int32               -- Image height in pixels
instance HasArgument Fractsurf "fractal-dimension" Double   -- Fractal dimension
instance HasOutput Fractsurf "out" GV.Image                 -- Output image

type Identity = VipsOp "identity" GV.Image
instance HasArgument Identity "bands" Int32                 -- Number of bands in LUT
instance HasArgument Identity "ushort" Bool                 -- Create a 16-bit LUT
instance HasArgument Identity "size" Int32                  -- Size of 16-bit LUT
instance HasOutput Identity "out" GV.Image                  -- Output image

type Tonelut = VipsOp "tonelut" GV.Image
instance HasArgument Tonelut "in-max" Int32                 -- Size of LUT to build
instance HasArgument Tonelut "out-max" Int32                -- Maximum value in output LUT
instance HasArgument Tonelut "Lb" Double                    -- Lowest value in output
instance HasArgument Tonelut "Lw" Double                    -- Highest value in output
instance HasArgument Tonelut "Ps" Double                    -- Position of shadow
instance HasArgument Tonelut "Pm" Double                    -- Position of mid-tones
instance HasArgument Tonelut "Ph" Double                    -- Position of highlights
instance HasArgument Tonelut "S" Double                     -- Adjust shadows by this much
instance HasArgument Tonelut "M" Double                     -- Adjust mid-tones by this much
instance HasArgument Tonelut "H" Double                     -- Adjust highlights by this much
instance HasOutput Tonelut "out" GV.Image                   -- Output image

type Invertlut = VipsOp "invertlut" GV.Image
instance HasArgument Invertlut "in" GV.Image                -- Matrix of XY coordinates
instance HasArgument Invertlut "size" Int32                 -- LUT size to generate
instance HasOutput Invertlut "out" GV.Image                 -- Output image

type Buildlut = VipsOp "buildlut" GV.Image
instance HasArgument Buildlut "in" GV.Image                 -- Matrix of XY coordinates
instance HasOutput Buildlut "out" GV.Image                  -- Output image

type MaskFractal = VipsOp "mask_fractal" GV.Image
instance HasArgument MaskFractal "width" Int32              -- Image width in pixels
instance HasArgument MaskFractal "height" Int32             -- Image height in pixels
instance HasArgument MaskFractal "uchar" Bool               -- Output an unsigned char image
instance HasArgument MaskFractal "optical" Bool             -- Rotate quadrants to optical space
instance HasArgument MaskFractal "reject" Bool              -- Invert the sense of the filter
instance HasArgument MaskFractal "nodc" Bool                -- Remove DC component
instance HasArgument MaskFractal "fractal-dimension" Double  -- Fractal dimension
instance HasOutput MaskFractal "out" GV.Image               -- Output image

type MaskGaussianBand = VipsOp "mask_gaussian_band" GV.Image
instance HasArgument MaskGaussianBand "width" Int32         -- Image width in pixels
instance HasArgument MaskGaussianBand "height" Int32        -- Image height in pixels
instance HasArgument MaskGaussianBand "uchar" Bool          -- Output an unsigned char image
instance HasArgument MaskGaussianBand "optical" Bool        -- Rotate quadrants to optical space
instance HasArgument MaskGaussianBand "reject" Bool         -- Invert the sense of the filter
instance HasArgument MaskGaussianBand "nodc" Bool           -- Remove DC component
instance HasArgument MaskGaussianBand "frequency-cutoff-x" Double  -- Frequency cutoff x
instance HasArgument MaskGaussianBand "frequency-cutoff-y" Double  -- Frequency cutoff y
instance HasArgument MaskGaussianBand "radius" Double       -- radius of circle
instance HasArgument MaskGaussianBand "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasOutput MaskGaussianBand "out" GV.Image          -- Output image

type MaskGaussianRing = VipsOp "mask_gaussian_ring" GV.Image
instance HasArgument MaskGaussianRing "width" Int32         -- Image width in pixels
instance HasArgument MaskGaussianRing "height" Int32        -- Image height in pixels
instance HasArgument MaskGaussianRing "uchar" Bool          -- Output an unsigned char image
instance HasArgument MaskGaussianRing "optical" Bool        -- Rotate quadrants to optical space
instance HasArgument MaskGaussianRing "reject" Bool         -- Invert the sense of the filter
instance HasArgument MaskGaussianRing "nodc" Bool           -- Remove DC component
instance HasArgument MaskGaussianRing "frequency-cutoff" Double  -- Frequency cutoff
instance HasArgument MaskGaussianRing "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasArgument MaskGaussianRing "ringwidth" Double    -- Ringwidth
instance HasOutput MaskGaussianRing "out" GV.Image          -- Output image

type MaskGaussian = VipsOp "mask_gaussian" GV.Image
instance HasArgument MaskGaussian "width" Int32             -- Image width in pixels
instance HasArgument MaskGaussian "height" Int32            -- Image height in pixels
instance HasArgument MaskGaussian "uchar" Bool              -- Output an unsigned char image
instance HasArgument MaskGaussian "optical" Bool            -- Rotate quadrants to optical space
instance HasArgument MaskGaussian "reject" Bool             -- Invert the sense of the filter
instance HasArgument MaskGaussian "nodc" Bool               -- Remove DC component
instance HasArgument MaskGaussian "frequency-cutoff" Double  -- Frequency cutoff
instance HasArgument MaskGaussian "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasOutput MaskGaussian "out" GV.Image              -- Output image

type MaskButterworthBand = VipsOp "mask_butterworth_band" GV.Image
instance HasArgument MaskButterworthBand "width" Int32      -- Image width in pixels
instance HasArgument MaskButterworthBand "height" Int32     -- Image height in pixels
instance HasArgument MaskButterworthBand "uchar" Bool       -- Output an unsigned char image
instance HasArgument MaskButterworthBand "optical" Bool     -- Rotate quadrants to optical space
instance HasArgument MaskButterworthBand "reject" Bool      -- Invert the sense of the filter
instance HasArgument MaskButterworthBand "nodc" Bool        -- Remove DC component
instance HasArgument MaskButterworthBand "order" Double     -- Filter order
instance HasArgument MaskButterworthBand "frequency-cutoff-x" Double  -- Frequency cutoff x
instance HasArgument MaskButterworthBand "frequency-cutoff-y" Double  -- Frequency cutoff y
instance HasArgument MaskButterworthBand "radius" Double    -- radius of circle
instance HasArgument MaskButterworthBand "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasOutput MaskButterworthBand "out" GV.Image       -- Output image

type MaskButterworthRing = VipsOp "mask_butterworth_ring" GV.Image
instance HasArgument MaskButterworthRing "width" Int32      -- Image width in pixels
instance HasArgument MaskButterworthRing "height" Int32     -- Image height in pixels
instance HasArgument MaskButterworthRing "uchar" Bool       -- Output an unsigned char image
instance HasArgument MaskButterworthRing "optical" Bool     -- Rotate quadrants to optical space
instance HasArgument MaskButterworthRing "reject" Bool      -- Invert the sense of the filter
instance HasArgument MaskButterworthRing "nodc" Bool        -- Remove DC component
instance HasArgument MaskButterworthRing "order" Double     -- Filter order
instance HasArgument MaskButterworthRing "frequency-cutoff" Double  -- Frequency cutoff
instance HasArgument MaskButterworthRing "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasArgument MaskButterworthRing "ringwidth" Double  -- Ringwidth
instance HasOutput MaskButterworthRing "out" GV.Image       -- Output image

type MaskButterworth = VipsOp "mask_butterworth" GV.Image
instance HasArgument MaskButterworth "width" Int32          -- Image width in pixels
instance HasArgument MaskButterworth "height" Int32         -- Image height in pixels
instance HasArgument MaskButterworth "uchar" Bool           -- Output an unsigned char image
instance HasArgument MaskButterworth "optical" Bool         -- Rotate quadrants to optical space
instance HasArgument MaskButterworth "reject" Bool          -- Invert the sense of the filter
instance HasArgument MaskButterworth "nodc" Bool            -- Remove DC component
instance HasArgument MaskButterworth "order" Double         -- Filter order
instance HasArgument MaskButterworth "frequency-cutoff" Double  -- Frequency cutoff
instance HasArgument MaskButterworth "amplitude-cutoff" Double  -- Amplitude cutoff
instance HasOutput MaskButterworth "out" GV.Image           -- Output image

type MaskIdealBand = VipsOp "mask_ideal_band" GV.Image
instance HasArgument MaskIdealBand "width" Int32            -- Image width in pixels
instance HasArgument MaskIdealBand "height" Int32           -- Image height in pixels
instance HasArgument MaskIdealBand "uchar" Bool             -- Output an unsigned char image
instance HasArgument MaskIdealBand "optical" Bool           -- Rotate quadrants to optical space
instance HasArgument MaskIdealBand "reject" Bool            -- Invert the sense of the filter
instance HasArgument MaskIdealBand "nodc" Bool              -- Remove DC component
instance HasArgument MaskIdealBand "frequency-cutoff-x" Double  -- Frequency cutoff x
instance HasArgument MaskIdealBand "frequency-cutoff-y" Double  -- Frequency cutoff y
instance HasArgument MaskIdealBand "radius" Double          -- radius of circle
instance HasOutput MaskIdealBand "out" GV.Image             -- Output image

type MaskIdealRing = VipsOp "mask_ideal_ring" GV.Image
instance HasArgument MaskIdealRing "width" Int32            -- Image width in pixels
instance HasArgument MaskIdealRing "height" Int32           -- Image height in pixels
instance HasArgument MaskIdealRing "uchar" Bool             -- Output an unsigned char image
instance HasArgument MaskIdealRing "optical" Bool           -- Rotate quadrants to optical space
instance HasArgument MaskIdealRing "reject" Bool            -- Invert the sense of the filter
instance HasArgument MaskIdealRing "nodc" Bool              -- Remove DC component
instance HasArgument MaskIdealRing "frequency-cutoff" Double  -- Frequency cutoff
instance HasArgument MaskIdealRing "ringwidth" Double       -- Ringwidth
instance HasOutput MaskIdealRing "out" GV.Image             -- Output image

type MaskIdeal = VipsOp "mask_ideal" GV.Image
instance HasArgument MaskIdeal "width" Int32                -- Image width in pixels
instance HasArgument MaskIdeal "height" Int32               -- Image height in pixels
instance HasArgument MaskIdeal "uchar" Bool                 -- Output an unsigned char image
instance HasArgument MaskIdeal "optical" Bool               -- Rotate quadrants to optical space
instance HasArgument MaskIdeal "reject" Bool                -- Invert the sense of the filter
instance HasArgument MaskIdeal "nodc" Bool                  -- Remove DC component
instance HasArgument MaskIdeal "frequency-cutoff" Double    -- Frequency cutoff
instance HasOutput MaskIdeal "out" GV.Image                 -- Output image

type Sines = VipsOp "sines" GV.Image
instance HasArgument Sines "width" Int32                    -- Image width in pixels
instance HasArgument Sines "height" Int32                   -- Image height in pixels
instance HasArgument Sines "uchar" Bool                     -- Output an unsigned char image
instance HasArgument Sines "hfreq" Double                   -- Horizontal spatial frequency
instance HasArgument Sines "vfreq" Double                   -- Vertical spatial frequency
instance HasOutput Sines "out" GV.Image                     -- Output image

type Zone = VipsOp "zone" GV.Image
instance HasArgument Zone "width" Int32                     -- Image width in pixels
instance HasArgument Zone "height" Int32                    -- Image height in pixels
instance HasArgument Zone "uchar" Bool                      -- Output an unsigned char image
instance HasOutput Zone "out" GV.Image                      -- Output image

type Grey = VipsOp "grey" GV.Image
instance HasArgument Grey "width" Int32                     -- Image width in pixels
instance HasArgument Grey "height" Int32                    -- Image height in pixels
instance HasArgument Grey "uchar" Bool                      -- Output an unsigned char image
instance HasOutput Grey "out" GV.Image                      -- Output image

type Eye = VipsOp "eye" GV.Image
instance HasArgument Eye "width" Int32                      -- Image width in pixels
instance HasArgument Eye "height" Int32                     -- Image height in pixels
instance HasArgument Eye "uchar" Bool                       -- Output an unsigned char image
instance HasArgument Eye "factor" Double                    -- Maximum spatial frequency
instance HasOutput Eye "out" GV.Image                       -- Output image

type Logmat = VipsOp "logmat" GV.Image
instance HasArgument Logmat "sigma" Double                  -- Radius of Logmatian
instance HasArgument Logmat "min-ampl" Double               -- Minimum amplitude of Logmatian
instance HasArgument Logmat "separable" Bool                -- Generate separable Logmatian
instance HasArgument Logmat "precision" GV.Precision        -- Generate with this precision
instance HasOutput Logmat "out" GV.Image                    -- Output image

type Gaussmat = VipsOp "gaussmat" GV.Image
instance HasArgument Gaussmat "sigma" Double                -- Sigma of Gaussian
instance HasArgument Gaussmat "min-ampl" Double             -- Minimum amplitude of Gaussian
instance HasArgument Gaussmat "separable" Bool              -- Generate separable Gaussian
instance HasArgument Gaussmat "precision" GV.Precision      -- Generate with this precision
instance HasOutput Gaussmat "out" GV.Image                  -- Output image

type Xyz = VipsOp "xyz" GV.Image
instance HasArgument Xyz "width" Int32                      -- Image width in pixels
instance HasArgument Xyz "height" Int32                     -- Image height in pixels
instance HasArgument Xyz "csize" Int32                      -- Size of third dimension
instance HasArgument Xyz "dsize" Int32                      -- Size of fourth dimension
instance HasArgument Xyz "esize" Int32                      -- Size of fifth dimension
instance HasOutput Xyz "out" GV.Image                       -- Output image

type Text = VipsOp "text" TextResult
data TextResult = TextResult { out :: GV.Image, autofit_dpi :: Int32 }
instance HasArgument Text "text" T.Text                     -- Text to render
instance HasArgument Text "font" T.Text                     -- Font to render with
instance HasArgument Text "width" Int32                     -- Maximum image width in pixels
instance HasArgument Text "height" Int32                    -- Maximum image height in pixels
instance HasArgument Text "align" GV.Align                  -- Align on the low, centre or high edge
instance HasArgument Text "justify" Bool                    -- Justify lines
instance HasArgument Text "dpi" Int32                       -- DPI to render at
instance HasArgument Text "spacing" Int32                   -- Line spacing
instance HasArgument Text "fontfile" T.Text                 -- Load this font file
instance HasOutput Text "out" GV.Image                      -- Output image
instance HasOutput Text "autofit-dpi" Int32                 -- DPI selected by autofit

type Gaussnoise = VipsOp "gaussnoise" GV.Image
instance HasArgument Gaussnoise "width" Int32               -- Image width in pixels
instance HasArgument Gaussnoise "height" Int32              -- Image height in pixels
instance HasArgument Gaussnoise "mean" Double               -- Mean of pixels in generated image
instance HasArgument Gaussnoise "sigma" Double              -- Standard deviation of pixels in generated image
instance HasOutput Gaussnoise "out" GV.Image                -- Output image

type Black = VipsOp "black" GV.Image
instance HasArgument Black "width" Int32                    -- Image width in pixels
instance HasArgument Black "height" Int32                   -- Image height in pixels
instance HasArgument Black "bands" Int32                    -- Number of bands in image
instance HasOutput Black "out" GV.Image                     -- Output image

type Composite2 = VipsOp "composite2" GV.Image
instance HasArgument Composite2 "base" GV.Image             -- Base image
instance HasArgument Composite2 "overlay" GV.Image          -- Overlay image
instance HasArgument Composite2 "mode" GV.BlendMode         -- VipsBlendMode to join with
instance HasArgument Composite2 "x" Int32                   -- x position of overlay
instance HasArgument Composite2 "y" Int32                   -- y position of overlay
instance HasArgument Composite2 "compositing-space" GV.Interpretation  -- Composite images in this colour space
instance HasArgument Composite2 "premultiplied" Bool        -- Images have premultiplied alpha
instance HasOutput Composite2 "out" GV.Image                -- Output image

type Composite = VipsOp "composite" GV.Image
instance HasArgument Composite "in" GV.ArrayImage           -- Array of input images
instance HasArgument Composite "mode" GV.ArrayInt           -- Array of VipsBlendMode to join with
instance HasArgument Composite "x" GV.ArrayInt              -- Array of x coordinates to join at
instance HasArgument Composite "y" GV.ArrayInt              -- Array of y coordinates to join at
instance HasArgument Composite "compositing-space" GV.Interpretation  -- Composite images in this colour space
instance HasArgument Composite "premultiplied" Bool         -- Images have premultiplied alpha
instance HasOutput Composite "out" GV.Image                 -- Output image

type Gamma = VipsOp "gamma" GV.Image
instance HasArgument Gamma "in" GV.Image                    -- Input image
instance HasArgument Gamma "exponent" Double                -- Gamma factor
instance HasOutput Gamma "out" GV.Image                     -- Output image

type Falsecolour = VipsOp "falsecolour" GV.Image
instance HasArgument Falsecolour "in" GV.Image              -- Input image
instance HasOutput Falsecolour "out" GV.Image               -- Output image

type Byteswap = VipsOp "byteswap" GV.Image
instance HasArgument Byteswap "in" GV.Image                 -- Input image
instance HasOutput Byteswap "out" GV.Image                  -- Output image

type Msb = VipsOp "msb" GV.Image
instance HasArgument Msb "in" GV.Image                      -- Input image
instance HasArgument Msb "band" Int32                       -- Band to msb
instance HasOutput Msb "out" GV.Image                       -- Output image

type Subsample = VipsOp "subsample" GV.Image
instance HasArgument Subsample "input" GV.Image             -- Input image
instance HasArgument Subsample "xfac" Int32                 -- Horizontal subsample factor
instance HasArgument Subsample "yfac" Int32                 -- Vertical subsample factor
instance HasArgument Subsample "point" Bool                 -- Point sample
instance HasOutput Subsample "out" GV.Image                 -- Output image

type Zoom = VipsOp "zoom" GV.Image
instance HasArgument Zoom "input" GV.Image                  -- Input image
instance HasArgument Zoom "xfac" Int32                      -- Horizontal zoom factor
instance HasArgument Zoom "yfac" Int32                      -- Vertical zoom factor
instance HasOutput Zoom "out" GV.Image                      -- Output image

type Wrap = VipsOp "wrap" GV.Image
instance HasArgument Wrap "in" GV.Image                     -- Input image
instance HasArgument Wrap "x" Int32                         -- Left edge of input in output
instance HasArgument Wrap "y" Int32                         -- Top edge of input in output
instance HasOutput Wrap "out" GV.Image                      -- Output image

type Scale = VipsOp "scale" GV.Image
instance HasArgument Scale "in" GV.Image                    -- Input image
instance HasArgument Scale "log" Bool                       -- Log scale
instance HasArgument Scale "exp" Double                     -- Exponent for log scale
instance HasOutput Scale "out" GV.Image                     -- Output image

type Transpose3d = VipsOp "transpose3d" GV.Image
instance HasArgument Transpose3d "in" GV.Image              -- Input image
instance HasArgument Transpose3d "page-height" Int32        -- Height of each input page
instance HasOutput Transpose3d "out" GV.Image               -- Output image

type Grid = VipsOp "grid" GV.Image
instance HasArgument Grid "in" GV.Image                     -- Input image
instance HasArgument Grid "tile-height" Int32               -- chop into tiles this high
instance HasArgument Grid "across" Int32                    -- number of tiles across
instance HasArgument Grid "down" Int32                      -- number of tiles down
instance HasOutput Grid "out" GV.Image                      -- Output image

type Unpremultiply = VipsOp "unpremultiply" GV.Image
instance HasArgument Unpremultiply "in" GV.Image            -- Input image
instance HasArgument Unpremultiply "max-alpha" Double       -- Maximum value of alpha channel
instance HasArgument Unpremultiply "alpha-band" Int32       -- Unpremultiply with this alpha
instance HasOutput Unpremultiply "out" GV.Image             -- Output image

type Premultiply = VipsOp "premultiply" GV.Image
instance HasArgument Premultiply "in" GV.Image              -- Input image
instance HasArgument Premultiply "max-alpha" Double         -- Maximum value of alpha channel
instance HasOutput Premultiply "out" GV.Image               -- Output image

type Flatten = VipsOp "flatten" GV.Image
instance HasArgument Flatten "in" GV.Image                  -- Input image
instance HasArgument Flatten "background" GV.ArrayDouble    -- Background value
instance HasArgument Flatten "max-alpha" Double             -- Maximum value of alpha channel
instance HasOutput Flatten "out" GV.Image                   -- Output image

type Bandunfold = VipsOp "bandunfold" GV.Image
instance HasArgument Bandunfold "in" GV.Image               -- Input image
instance HasArgument Bandunfold "factor" Int32              -- Unfold by this factor
instance HasOutput Bandunfold "out" GV.Image                -- Output image

type Bandfold = VipsOp "bandfold" GV.Image
instance HasArgument Bandfold "in" GV.Image                 -- Input image
instance HasArgument Bandfold "factor" Int32                -- Fold by this factor
instance HasOutput Bandfold "out" GV.Image                  -- Output image

type Recomb = VipsOp "recomb" GV.Image
instance HasArgument Recomb "in" GV.Image                   -- Input image argument
instance HasArgument Recomb "m" GV.Image                    -- matrix of coefficients
instance HasOutput Recomb "out" GV.Image                    -- Output image

type Ifthenelse = VipsOp "ifthenelse" GV.Image
instance HasArgument Ifthenelse "cond" GV.Image             -- Condition input image
instance HasArgument Ifthenelse "in1" GV.Image              -- Source for TRUE pixels
instance HasArgument Ifthenelse "in2" GV.Image              -- Source for FALSE pixels
instance HasArgument Ifthenelse "blend" Bool                -- Blend smoothly between then and else parts
instance HasOutput Ifthenelse "out" GV.Image                -- Output image

type Autorot = VipsOp "autorot" AutorotResult
data AutorotResult = AutorotResult { out :: GV.Image, angle :: GV.Angle, flip :: Bool }
instance HasArgument Autorot "in" GV.Image                  -- Input image
instance HasOutput Autorot "out" GV.Image                   -- Output image
instance HasOutput Autorot "angle" GV.Angle                 -- Angle image was rotated by
instance HasOutput Autorot "flip" Bool                      -- Whether the image was flipped or not

type Rot45 = VipsOp "rot45" GV.Image
instance HasArgument Rot45 "in" GV.Image                    -- Input image
instance HasArgument Rot45 "angle" GV.Angle45               -- Angle to rotate image
instance HasOutput Rot45 "out" GV.Image                     -- Output image

type Rot = VipsOp "rot" GV.Image
instance HasArgument Rot "in" GV.Image                      -- Input image
instance HasArgument Rot "angle" GV.Angle                   -- Angle to rotate image
instance HasOutput Rot "out" GV.Image                       -- Output image

type Cast = VipsOp "cast" GV.Image
instance HasArgument Cast "in" GV.Image                     -- Input image
instance HasArgument Cast "format" GV.BandFormat            -- Format to cast to
instance HasArgument Cast "shift" Bool                      -- Shift integer values up and down
instance HasOutput Cast "out" GV.Image                      -- Output image

type Replicate = VipsOp "replicate" GV.Image
instance HasArgument Replicate "in" GV.Image                -- Input image
instance HasArgument Replicate "across" Int32               -- Repeat this many times horizontally
instance HasArgument Replicate "down" Int32                 -- Repeat this many times vertically
instance HasOutput Replicate "out" GV.Image                 -- Output image

type Bandbool = VipsOp "bandbool" GV.Image
instance HasArgument Bandbool "in" GV.Image                 -- Input image argument
instance HasArgument Bandbool "boolean" GV.OperationBoolean  -- boolean to perform
instance HasOutput Bandbool "out" GV.Image                  -- Output image

type Bandmean = VipsOp "bandmean" GV.Image
instance HasArgument Bandmean "in" GV.Image                 -- Input image argument
instance HasOutput Bandmean "out" GV.Image                  -- Output image

type Bandrank = VipsOp "bandrank" GV.Image
instance HasArgument Bandrank "in" GV.ArrayImage            -- Array of input images
instance HasArgument Bandrank "index" Int32                 -- Select this band element from sorted list
instance HasOutput Bandrank "out" GV.Image                  -- Output image

type BandjoinConst = VipsOp "bandjoin_const" GV.Image
instance HasArgument BandjoinConst "in" GV.Image            -- Input image
instance HasArgument BandjoinConst "c" GV.ArrayDouble       -- Array of constants to add
instance HasOutput BandjoinConst "out" GV.Image             -- Output image

type Bandjoin = VipsOp "bandjoin" GV.Image
instance HasArgument Bandjoin "in" GV.ArrayImage            -- Array of input images
instance HasOutput Bandjoin "out" GV.Image                  -- Output image

type ExtractBand = VipsOp "extract_band" GV.Image
instance HasArgument ExtractBand "in" GV.Image              -- Input image
instance HasArgument ExtractBand "band" Int32               -- Band to extract
instance HasArgument ExtractBand "n" Int32                  -- Number of bands to extract
instance HasOutput ExtractBand "out" GV.Image               -- Output image

type Smartcrop = VipsOp "smartcrop" GV.Image
instance HasArgument Smartcrop "input" GV.Image             -- Input image
instance HasArgument Smartcrop "width" Int32                -- Width of extract area
instance HasArgument Smartcrop "height" Int32               -- Height of extract area
instance HasArgument Smartcrop "interesting" GV.Interesting  -- How to measure interestingness
instance HasOutput Smartcrop "out" GV.Image                 -- Output image

type ExtractArea = VipsOp "extract_area" GV.Image
instance HasArgument ExtractArea "input" GV.Image           -- Input image
instance HasArgument ExtractArea "left" Int32               -- Left edge of extract area
instance HasArgument ExtractArea "top" Int32                -- Top edge of extract area
instance HasArgument ExtractArea "width" Int32              -- Width of extract area
instance HasArgument ExtractArea "height" Int32             -- Height of extract area
instance HasOutput ExtractArea "out" GV.Image               -- Output image

type Arrayjoin = VipsOp "arrayjoin" GV.Image
instance HasArgument Arrayjoin "in" GV.ArrayImage           -- Array of input images
instance HasArgument Arrayjoin "across" Int32               -- Number of images across grid
instance HasArgument Arrayjoin "shim" Int32                 -- Pixels between images
instance HasArgument Arrayjoin "background" GV.ArrayDouble  -- Colour for new pixels
instance HasArgument Arrayjoin "halign" GV.Align            -- Align on the left, centre or right
instance HasArgument Arrayjoin "valign" GV.Align            -- Align on the top, centre or bottom
instance HasArgument Arrayjoin "hspacing" Int32             -- Horizontal spacing between images
instance HasArgument Arrayjoin "vspacing" Int32             -- Vertical spacing between images
instance HasOutput Arrayjoin "out" GV.Image                 -- Output image

type Join = VipsOp "join" GV.Image
instance HasArgument Join "in1" GV.Image                    -- First input image
instance HasArgument Join "in2" GV.Image                    -- Second input image
instance HasArgument Join "direction" GV.Direction          -- Join left-right or up-down
instance HasArgument Join "expand" Bool                     -- Expand output to hold all of both inputs
instance HasArgument Join "shim" Int32                      -- Pixels between images
instance HasArgument Join "background" GV.ArrayDouble       -- Colour for new pixels
instance HasArgument Join "align" GV.Align                  -- Align on the low, centre or high coordinate edge
instance HasOutput Join "out" GV.Image                      -- Output image

type Insert = VipsOp "insert" GV.Image
instance HasArgument Insert "main" GV.Image                 -- Main input image
instance HasArgument Insert "sub" GV.Image                  -- Sub-image to insert into main image
instance HasArgument Insert "x" Int32                       -- Left edge of sub in main
instance HasArgument Insert "y" Int32                       -- Top edge of sub in main
instance HasArgument Insert "expand" Bool                   -- Expand output to hold all of both inputs
instance HasArgument Insert "background" GV.ArrayDouble     -- Color for new pixels
instance HasOutput Insert "out" GV.Image                    -- Output image

type Flip = VipsOp "flip" GV.Image
instance HasArgument Flip "in" GV.Image                     -- Input image
instance HasArgument Flip "direction" GV.Direction          -- Direction to flip image
instance HasOutput Flip "out" GV.Image                      -- Output image

type Gravity = VipsOp "gravity" GV.Image
instance HasArgument Gravity "in" GV.Image                  -- Input image
instance HasArgument Gravity "direction" GV.CompassDirection  -- direction to place image within width/height
instance HasArgument Gravity "width" Int32                  -- Image width in pixels
instance HasArgument Gravity "height" Int32                 -- Image height in pixels
instance HasArgument Gravity "extend" GV.Extend             -- How to generate the extra pixels
instance HasArgument Gravity "background" GV.ArrayDouble    -- Color for background pixels
instance HasOutput Gravity "out" GV.Image                   -- Output image

type Embed = VipsOp "embed" GV.Image
instance HasArgument Embed "in" GV.Image                    -- Input image
instance HasArgument Embed "x" Int32                        -- Left edge of input in output
instance HasArgument Embed "y" Int32                        -- Top edge of input in output
instance HasArgument Embed "width" Int32                    -- Image width in pixels
instance HasArgument Embed "height" Int32                   -- Image height in pixels
instance HasArgument Embed "extend" GV.Extend               -- How to generate the extra pixels
instance HasArgument Embed "background" GV.ArrayDouble      -- Color for background pixels
instance HasOutput Embed "out" GV.Image                     -- Output image

type Cache = VipsOp "cache" GV.Image
instance HasArgument Cache "in" GV.Image                    -- Input image
instance HasArgument Cache "tile-width" Int32               -- Tile width in pixels
instance HasArgument Cache "tile-height" Int32              -- Tile height in pixels
instance HasArgument Cache "max-tiles" Int32                -- Maximum number of tiles to cache
instance HasOutput Cache "out" GV.Image                     -- Output image

type Sequential = VipsOp "sequential" GV.Image
instance HasArgument Sequential "in" GV.Image               -- Input image
instance HasArgument Sequential "tile-height" Int32         -- Tile height in pixels
instance HasOutput Sequential "out" GV.Image                -- Output image

type Linecache = VipsOp "linecache" GV.Image
instance HasArgument Linecache "in" GV.Image                -- Input image
instance HasArgument Linecache "tile-height" Int32          -- Tile height in pixels
instance HasArgument Linecache "access" GV.Access           -- Expected access pattern
instance HasArgument Linecache "threaded" Bool              -- Allow threaded access
instance HasArgument Linecache "persistent" Bool            -- Keep cache between evaluations
instance HasOutput Linecache "out" GV.Image                 -- Output image

type Tilecache = VipsOp "tilecache" GV.Image
instance HasArgument Tilecache "in" GV.Image                -- Input image
instance HasArgument Tilecache "tile-width" Int32           -- Tile width in pixels
instance HasArgument Tilecache "tile-height" Int32          -- Tile height in pixels
instance HasArgument Tilecache "max-tiles" Int32            -- Maximum number of tiles to cache
instance HasArgument Tilecache "access" GV.Access           -- Expected access pattern
instance HasArgument Tilecache "threaded" Bool              -- Allow threaded access
instance HasArgument Tilecache "persistent" Bool            -- Keep cache between evaluations
instance HasOutput Tilecache "out" GV.Image                 -- Output image

type Copy = VipsOp "copy" GV.Image
instance HasArgument Copy "in" GV.Image                     -- Input image
instance HasArgument Copy "width" Int32                     -- Image width in pixels
instance HasArgument Copy "height" Int32                    -- Image height in pixels
instance HasArgument Copy "bands" Int32                     -- Number of bands in image
instance HasArgument Copy "format" GV.BandFormat            -- Pixel format in image
instance HasArgument Copy "coding" GV.Coding                -- Pixel coding
instance HasArgument Copy "interpretation" GV.Interpretation  -- Pixel interpretation
instance HasArgument Copy "xres" Double                     -- Horizontal resolution in pixels/mm
instance HasArgument Copy "yres" Double                     -- Vertical resolution in pixels/mm
instance HasArgument Copy "xoffset" Int32                   -- Horizontal offset of origin
instance HasArgument Copy "yoffset" Int32                   -- Vertical offset of origin
instance HasOutput Copy "out" GV.Image                      -- Output image

type FindTrim = VipsOp "find_trim" FindTrimResult
data FindTrimResult = FindTrimResult { left :: Int32, top :: Int32, width :: Int32, height :: Int32 }
instance HasArgument FindTrim "in" GV.Image                 -- Image to find_trim
instance HasArgument FindTrim "threshold" Double            -- Object threshold
instance HasArgument FindTrim "background" GV.ArrayDouble   -- Color for background pixels
instance HasOutput FindTrim "left" Int32                    -- Left edge of image
instance HasOutput FindTrim "top" Int32                     -- Top edge of extract area
instance HasOutput FindTrim "width" Int32                   -- Width of extract area
instance HasOutput FindTrim "height" Int32                  -- Height of extract area

type Getpoint = VipsOp "getpoint" GV.ArrayDouble
instance HasArgument Getpoint "in" GV.Image                 -- Input image
instance HasArgument Getpoint "x" Int32                     -- Point to read
instance HasArgument Getpoint "y" Int32                     -- Point to read
instance HasOutput Getpoint "out-array" GV.ArrayDouble      -- Array of output values

type Measure = VipsOp "measure" GV.Image
instance HasArgument Measure "in" GV.Image                  -- Image to measure
instance HasArgument Measure "h" Int32                      -- Number of patches across chart
instance HasArgument Measure "v" Int32                      -- Number of patches down chart
instance HasArgument Measure "left" Int32                   -- Left edge of extract area
instance HasArgument Measure "top" Int32                    -- Top edge of extract area
instance HasArgument Measure "width" Int32                  -- Width of extract area
instance HasArgument Measure "height" Int32                 -- Height of extract area
instance HasOutput Measure "out" GV.Image                   -- Output array of statistics

type Profile = VipsOp "profile" ProfileResult
data ProfileResult = ProfileResult { columns :: GV.Image, rows :: GV.Image }
instance HasArgument Profile "in" GV.Image                  -- Input image
instance HasOutput Profile "columns" GV.Image               -- First non-zero pixel in column
instance HasOutput Profile "rows" GV.Image                  -- First non-zero pixel in row

type Project = VipsOp "project" ProjectResult
data ProjectResult = ProjectResult { columns :: GV.Image, rows :: GV.Image }
instance HasArgument Project "in" GV.Image                  -- Input image
instance HasOutput Project "columns" GV.Image               -- Sums of columns
instance HasOutput Project "rows" GV.Image                  -- Sums of rows

type HoughCircle = VipsOp "hough_circle" GV.Image
instance HasArgument HoughCircle "in" GV.Image              -- Input image
instance HasArgument HoughCircle "scale" Int32              -- Scale down dimensions by this factor
instance HasArgument HoughCircle "min-radius" Int32         -- Smallest radius to search for
instance HasArgument HoughCircle "max-radius" Int32         -- Largest radius to search for
instance HasOutput HoughCircle "out" GV.Image               -- Output image

type HoughLine = VipsOp "hough_line" GV.Image
instance HasArgument HoughLine "in" GV.Image                -- Input image
instance HasArgument HoughLine "width" Int32                -- horizontal size of parameter space
instance HasArgument HoughLine "height" Int32               -- Vertical size of parameter space
instance HasOutput HoughLine "out" GV.Image                 -- Output image

type HistFindIndexed = VipsOp "hist_find_indexed" GV.Image
instance HasArgument HistFindIndexed "in" GV.Image          -- Input image
instance HasArgument HistFindIndexed "index" GV.Image       -- Index image
instance HasArgument HistFindIndexed "combine" GV.Combine   -- Combine bins like this
instance HasOutput HistFindIndexed "out" GV.Image           -- Output histogram

type HistFindNdim = VipsOp "hist_find_ndim" GV.Image
instance HasArgument HistFindNdim "in" GV.Image             -- Input image
instance HasArgument HistFindNdim "bins" Int32              -- Number of bins in each dimension
instance HasOutput HistFindNdim "out" GV.Image              -- Output histogram

type HistFind = VipsOp "hist_find" GV.Image
instance HasArgument HistFind "in" GV.Image                 -- Input image
instance HasArgument HistFind "band" Int32                  -- Find histogram of band
instance HasOutput HistFind "out" GV.Image                  -- Output histogram

type Stats = VipsOp "stats" GV.Image
instance HasArgument Stats "in" GV.Image                    -- Input image
instance HasOutput Stats "out" GV.Image                     -- Output array of statistics

type Deviate = VipsOp "deviate" Double
instance HasArgument Deviate "in" GV.Image                  -- Input image
instance HasOutput Deviate "out" Double                     -- Output value

type Max = VipsOp "max" MaxResult
data MaxResult = MaxResult { out :: Double, x :: Int32, y :: Int32, out_array :: GV.ArrayDouble, x_array :: GV.ArrayInt, y_array :: GV.ArrayInt }
instance HasArgument Max "in" GV.Image                      -- Input image
instance HasArgument Max "size" Int32                       -- Number of maximum values to find
instance HasOutput Max "out" Double                         -- Output value
instance HasOutput Max "x" Int32                            -- Horizontal position of maximum
instance HasOutput Max "y" Int32                            -- Vertical position of maximum
instance HasOutput Max "out-array" GV.ArrayDouble           -- Array of output values
instance HasOutput Max "x-array" GV.ArrayInt                -- Array of horizontal positions
instance HasOutput Max "y-array" GV.ArrayInt                -- Array of vertical positions

type Min = VipsOp "min" MinResult
data MinResult = MinResult { out :: Double, x :: Int32, y :: Int32, out_array :: GV.ArrayDouble, x_array :: GV.ArrayInt, y_array :: GV.ArrayInt }
instance HasArgument Min "in" GV.Image                      -- Input image
instance HasArgument Min "size" Int32                       -- Number of minimum values to find
instance HasOutput Min "out" Double                         -- Output value
instance HasOutput Min "x" Int32                            -- Horizontal position of minimum
instance HasOutput Min "y" Int32                            -- Vertical position of minimum
instance HasOutput Min "out-array" GV.ArrayDouble           -- Array of output values
instance HasOutput Min "x-array" GV.ArrayInt                -- Array of horizontal positions
instance HasOutput Min "y-array" GV.ArrayInt                -- Array of vertical positions

type Avg = VipsOp "avg" Double
instance HasArgument Avg "in" GV.Image                      -- Input image
instance HasOutput Avg "out" Double                         -- Output value

type Complexget = VipsOp "complexget" GV.Image
instance HasArgument Complexget "in" GV.Image               -- Input image
instance HasArgument Complexget "get" GV.OperationComplexget  -- complex to perform
instance HasOutput Complexget "out" GV.Image                -- Output image

type Complex = VipsOp "complex" GV.Image
instance HasArgument Complex "in" GV.Image                  -- Input image
instance HasArgument Complex "cmplx" GV.OperationComplex    -- complex to perform
instance HasOutput Complex "out" GV.Image                   -- Output image

type Math2Const = VipsOp "math2_const" GV.Image
instance HasArgument Math2Const "in" GV.Image               -- Input image
instance HasArgument Math2Const "math2" GV.OperationMath2   -- math to perform
instance HasArgument Math2Const "c" GV.ArrayDouble          -- Array of constants
instance HasOutput Math2Const "out" GV.Image                -- Output image

type BooleanConst = VipsOp "boolean_const" GV.Image
instance HasArgument BooleanConst "in" GV.Image             -- Input image
instance HasArgument BooleanConst "boolean" GV.OperationBoolean  -- boolean to perform
instance HasArgument BooleanConst "c" GV.ArrayDouble        -- Array of constants
instance HasOutput BooleanConst "out" GV.Image              -- Output image

type RemainderConst = VipsOp "remainder_const" GV.Image
instance HasArgument RemainderConst "in" GV.Image           -- Input image
instance HasArgument RemainderConst "c" GV.ArrayDouble      -- Array of constants
instance HasOutput RemainderConst "out" GV.Image            -- Output image

type RelationalConst = VipsOp "relational_const" GV.Image
instance HasArgument RelationalConst "in" GV.Image          -- Input image
instance HasArgument RelationalConst "relational" GV.OperationRelational  -- relational to perform
instance HasArgument RelationalConst "c" GV.ArrayDouble     -- Array of constants
instance HasOutput RelationalConst "out" GV.Image           -- Output image

type Round = VipsOp "round" GV.Image
instance HasArgument Round "in" GV.Image                    -- Input image
instance HasArgument Round "round" GV.OperationRound        -- rounding operation to perform
instance HasOutput Round "out" GV.Image                     -- Output image

type Sign = VipsOp "sign" GV.Image
instance HasArgument Sign "in" GV.Image                     -- Input image
instance HasOutput Sign "out" GV.Image                      -- Output image

type Abs = VipsOp "abs" GV.Image
instance HasArgument Abs "in" GV.Image                      -- Input image
instance HasOutput Abs "out" GV.Image                       -- Output image

type Math = VipsOp "math" GV.Image
instance HasArgument Math "in" GV.Image                     -- Input image
instance HasArgument Math "math" GV.OperationMath           -- math to perform
instance HasOutput Math "out" GV.Image                      -- Output image

type Linear = VipsOp "linear" GV.Image
instance HasArgument Linear "in" GV.Image                   -- Input image
instance HasArgument Linear "a" GV.ArrayDouble              -- Multiply by this
instance HasArgument Linear "b" GV.ArrayDouble              -- Add this
instance HasArgument Linear "uchar" Bool                    -- Output should be uchar
instance HasOutput Linear "out" GV.Image                    -- Output image

type Invert = VipsOp "invert" GV.Image
instance HasArgument Invert "in" GV.Image                   -- Input image
instance HasOutput Invert "out" GV.Image                    -- Output image

type Sum = VipsOp "sum" GV.Image
instance HasArgument Sum "in" GV.ArrayImage                 -- Array of input images
instance HasOutput Sum "out" GV.Image                       -- Output image

type Complexform = VipsOp "complexform" GV.Image
instance HasArgument Complexform "left" GV.Image            -- Left-hand image argument
instance HasArgument Complexform "right" GV.Image           -- Right-hand image argument
instance HasOutput Complexform "out" GV.Image               -- Output image

type Complex2 = VipsOp "complex2" GV.Image
instance HasArgument Complex2 "left" GV.Image               -- Left-hand image argument
instance HasArgument Complex2 "right" GV.Image              -- Right-hand image argument
instance HasArgument Complex2 "cmplx" GV.OperationComplex2  -- binary complex operation to perform
instance HasOutput Complex2 "out" GV.Image                  -- Output image

type Math2 = VipsOp "math2" GV.Image
instance HasArgument Math2 "left" GV.Image                  -- Left-hand image argument
instance HasArgument Math2 "right" GV.Image                 -- Right-hand image argument
instance HasArgument Math2 "math2" GV.OperationMath2        -- math to perform
instance HasOutput Math2 "out" GV.Image                     -- Output image

type Boolean = VipsOp "boolean" GV.Image
instance HasArgument Boolean "left" GV.Image                -- Left-hand image argument
instance HasArgument Boolean "right" GV.Image               -- Right-hand image argument
instance HasArgument Boolean "boolean" GV.OperationBoolean  -- boolean to perform
instance HasOutput Boolean "out" GV.Image                   -- Output image

type Remainder = VipsOp "remainder" GV.Image
instance HasArgument Remainder "left" GV.Image              -- Left-hand image argument
instance HasArgument Remainder "right" GV.Image             -- Right-hand image argument
instance HasOutput Remainder "out" GV.Image                 -- Output image

type Relational = VipsOp "relational" GV.Image
instance HasArgument Relational "left" GV.Image             -- Left-hand image argument
instance HasArgument Relational "right" GV.Image            -- Right-hand image argument
instance HasArgument Relational "relational" GV.OperationRelational  -- relational to perform
instance HasOutput Relational "out" GV.Image                -- Output image

type Divide = VipsOp "divide" GV.Image
instance HasArgument Divide "left" GV.Image                 -- Left-hand image argument
instance HasArgument Divide "right" GV.Image                -- Right-hand image argument
instance HasOutput Divide "out" GV.Image                    -- Output image

type Multiply = VipsOp "multiply" GV.Image
instance HasArgument Multiply "left" GV.Image               -- Left-hand image argument
instance HasArgument Multiply "right" GV.Image              -- Right-hand image argument
instance HasOutput Multiply "out" GV.Image                  -- Output image

type Subtract = VipsOp "subtract" GV.Image
instance HasArgument Subtract "left" GV.Image               -- Left-hand image argument
instance HasArgument Subtract "right" GV.Image              -- Right-hand image argument
instance HasOutput Subtract "out" GV.Image                  -- Output image

type Add = VipsOp "add" GV.Image
instance HasArgument Add "left" GV.Image                    -- Left-hand image argument
instance HasArgument Add "right" GV.Image                   -- Right-hand image argument
instance HasOutput Add "out" GV.Image                       -- Output image

type System = VipsOp "system" SystemResult
data SystemResult = SystemResult { out :: GV.Image, log :: T.Text }
instance HasArgument System "in" GV.ArrayImage              -- Array of input images
instance HasArgument System "cmd-format" T.Text             -- Command to run
instance HasArgument System "in-format" T.Text              -- Format for input filename
instance HasArgument System "out-format" T.Text             -- Format for output filename
instance HasOutput System "out" GV.Image                    -- Output image
instance HasOutput System "log" T.Text                      -- Command log

type Crop = VipsOp "crop" GV.Image
instance HasArgument Crop "input" GV.Image                  -- Input image
instance HasArgument Crop "left" Int32                      -- Left edge of extract area
instance HasArgument Crop "top" Int32                       -- Top edge of extract area
instance HasArgument Crop "width" Int32                     -- Width of extract area
instance HasArgument Crop "height" Int32                    -- Height of extract area
instance HasOutput Crop "out" GV.Image                      -- Output image
