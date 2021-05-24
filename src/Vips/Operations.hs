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
    inputs = V.filename a'
    outputs = V.outImg

saveImage :: FilePath -> GV.Image -> SaveImage
saveImage a b = vipsForeignOp saver (Foreign :: Nickname "foreignSaveImage") & inputs & outputs
  where
    a' = T.pack a
    saver = GV.foreignFindSave a'
    inputs = V.filename a' . V.img b
    outputs = V.void

--
-- Vips image operations:
--

--
-- The following code has been automatically generated using hvips-gen,
-- from libvips 8.10.6-Tue May 18 11:07:36 UTC 2021
--


-- |global balance an image mosaic
globalbalance :: GV.Image ->  Globalbalance
globalbalance a = vipsOp (Lookup :: Nickname "globalbalance") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |first-order match of two images
match :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> GV.Image -> GV.Image ->  Match
match a b c d e f g h i j = vipsOp (Lookup :: Nickname "match") & inputs & outputs
  where
    inputs = V.ys2 a . V.xs2 b . V.yr2 c . V.xr2 d . V.ys1 e . V.xs1 f . V.yr1 g . V.xr1 h . V.sec i . V.ref j
    outputs = V.outImg

-- |invert an matrix
matrixinvert :: GV.Image ->  Matrixinvert
matrixinvert a = vipsOp (Lookup :: Nickname "matrixinvert") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |first-order mosaic of two images
mosaic1 :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Mosaic1
mosaic1 a b c d e f g h i j k = vipsOp (Lookup :: Nickname "mosaic1") & inputs & outputs
  where
    inputs = V.ys2 a . V.xs2 b . V.yr2 c . V.xr2 d . V.ys1 e . V.xs1 f . V.yr1 g . V.xr1 h . V.direction i . V.sec j . V.ref k
    outputs = V.outImg

-- |mosaic two images
mosaic :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Mosaic
mosaic a b c d e f g = vipsOp (Lookup :: Nickname "mosaic") & inputs & outputs
  where
    inputs = V.ysec a . V.xsec b . V.yref c . V.xref d . V.direction e . V.sec f . V.ref g
    outputs = V.outMosaicResult

-- |merge two images
merge :: Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Merge
merge a b c d e = vipsOp (Lookup :: Nickname "merge") & inputs & outputs
  where
    inputs = V.dy a . V.dx b . V.direction c . V.sec d . V.ref e
    outputs = V.outImg

-- |blur a rectangle on an image
drawSmudge :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  DrawSmudge
drawSmudge a b c d e = vipsOp (Lookup :: Nickname "draw_smudge") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.top c . V.left d . V.image e
    outputs = V.void

-- |paint an image into another image
drawImage :: Int32 -> Int32 -> GV.Image -> GV.Image ->  DrawImage
drawImage a b c d = vipsOp (Lookup :: Nickname "draw_image") & inputs & outputs
  where
    inputs = V.y a . V.x b . V.sub c . V.image d
    outputs = V.void

-- |flood-fill an area
drawFlood :: Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawFlood
drawFlood a b c d = vipsOp (Lookup :: Nickname "draw_flood") & inputs & outputs
  where
    inputs = V.y a . V.x b . V.ink c . V.image d
    outputs = V.outDrawFloodResult

-- |draw a circle on an image
drawCircle :: Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawCircle
drawCircle a b c d e = vipsOp (Lookup :: Nickname "draw_circle") & inputs & outputs
  where
    inputs = V.radius a . V.cy b . V.cx c . V.ink d . V.image e
    outputs = V.void

-- |draw a line on an image
drawLine :: Int32 -> Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawLine
drawLine a b c d e f = vipsOp (Lookup :: Nickname "draw_line") & inputs & outputs
  where
    inputs = V.y2 a . V.x2 b . V.y1 c . V.x1 d . V.ink e . V.image f
    outputs = V.void

-- |draw a mask on an image
drawMask :: Int32 -> Int32 -> GV.Image -> GV.ArrayDouble -> GV.Image ->  DrawMask
drawMask a b c d e = vipsOp (Lookup :: Nickname "draw_mask") & inputs & outputs
  where
    inputs = V.y a . V.x b . V.mask c . V.ink d . V.image e
    outputs = V.void

-- |paint a rectangle on an image
drawRect :: Int32 -> Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawRect
drawRect a b c d e f = vipsOp (Lookup :: Nickname "draw_rect") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.top c . V.left d . V.ink e . V.image f
    outputs = V.void

-- |fill image zeros with nearest non-zero pixel
fillNearest :: GV.Image ->  FillNearest
fillNearest a = vipsOp (Lookup :: Nickname "fill_nearest") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outFillNearestResult

-- |label regions in an image
labelregions :: GV.Image ->  Labelregions
labelregions a = vipsOp (Lookup :: Nickname "labelregions") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outLabelregionsResult

-- |count lines in an image
countlines :: GV.Direction -> GV.Image ->  Countlines
countlines a b = vipsOp (Lookup :: Nickname "countlines") & inputs & outputs
  where
    inputs = V.direction a . V.img b
    outputs = V.outNolines

-- |rank filter
rank :: Int32 -> Int32 -> Int32 -> GV.Image ->  Rank
rank a b c d = vipsOp (Lookup :: Nickname "rank") & inputs & outputs
  where
    inputs = V.index a . V.height b . V.width c . V.img d
    outputs = V.outImg

-- |morphology operation
morph :: GV.OperationMorphology -> GV.Image -> GV.Image ->  Morph
morph a b c = vipsOp (Lookup :: Nickname "morph") & inputs & outputs
  where
    inputs = V.morph a . V.mask b . V.img c
    outputs = V.outImg

-- |calculate phase correlation
phasecor :: GV.Image -> GV.Image ->  Phasecor
phasecor a b = vipsOp (Lookup :: Nickname "phasecor") & inputs & outputs
  where
    inputs = V.in2 a . V.img b
    outputs = V.outImg

-- |make displayable power spectrum
spectrum :: GV.Image ->  Spectrum
spectrum a = vipsOp (Lookup :: Nickname "spectrum") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |frequency-domain filtering
freqmult :: GV.Image -> GV.Image ->  Freqmult
freqmult a b = vipsOp (Lookup :: Nickname "freqmult") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |inverse FFT
invfft :: GV.Image ->  Invfft
invfft a = vipsOp (Lookup :: Nickname "invfft") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |forward FFT
fwfft :: GV.Image ->  Fwfft
fwfft a = vipsOp (Lookup :: Nickname "fwfft") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |Sobel edge detector
sobel :: GV.Image ->  Sobel
sobel a = vipsOp (Lookup :: Nickname "sobel") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |Canny edge detector
canny :: GV.Image ->  Canny
canny a = vipsOp (Lookup :: Nickname "canny") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |gaussian blur
gaussblur :: Double -> GV.Image ->  Gaussblur
gaussblur a b = vipsOp (Lookup :: Nickname "gaussblur") & inputs & outputs
  where
    inputs = V.sigma a . V.img b
    outputs = V.outImg

-- |unsharp masking for print
sharpen :: GV.Image ->  Sharpen
sharpen a = vipsOp (Lookup :: Nickname "sharpen") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |spatial correlation
spcor :: GV.Image -> GV.Image ->  Spcor
spcor a b = vipsOp (Lookup :: Nickname "spcor") & inputs & outputs
  where
    inputs = V.ref a . V.img b
    outputs = V.outImg

-- |fast correlation
fastcor :: GV.Image -> GV.Image ->  Fastcor
fastcor a b = vipsOp (Lookup :: Nickname "fastcor") & inputs & outputs
  where
    inputs = V.ref a . V.img b
    outputs = V.outImg

-- |approximate separable integer convolution
convasep :: GV.Image -> GV.Image ->  Convasep
convasep a b = vipsOp (Lookup :: Nickname "convasep") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |seperable convolution operation
convsep :: GV.Image -> GV.Image ->  Convsep
convsep a b = vipsOp (Lookup :: Nickname "convsep") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |convolve with rotating mask
compass :: GV.Image -> GV.Image ->  Compass
compass a b = vipsOp (Lookup :: Nickname "compass") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |int convolution operation
convi :: GV.Image -> GV.Image ->  Convi
convi a b = vipsOp (Lookup :: Nickname "convi") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |float convolution operation
convf :: GV.Image -> GV.Image ->  Convf
convf a b = vipsOp (Lookup :: Nickname "convf") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |approximate integer convolution
conva :: GV.Image -> GV.Image ->  Conva
conva a b = vipsOp (Lookup :: Nickname "conva") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |convolution operation
conv :: GV.Image -> GV.Image ->  Conv
conv a b = vipsOp (Lookup :: Nickname "conv") & inputs & outputs
  where
    inputs = V.mask a . V.img b
    outputs = V.outImg

-- |estimate image entropy
histEntropy :: GV.Image ->  HistEntropy
histEntropy a = vipsOp (Lookup :: Nickname "hist_entropy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

-- |test for monotonicity
histIsmonotonic :: GV.Image ->  HistIsmonotonic
histIsmonotonic a = vipsOp (Lookup :: Nickname "hist_ismonotonic") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMonotonic

-- |local histogram equalisation
histLocal :: Int32 -> Int32 -> GV.Image ->  HistLocal
histLocal a b c = vipsOp (Lookup :: Nickname "hist_local") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.img c
    outputs = V.outImg

-- |plot histogram
histPlot :: GV.Image ->  HistPlot
histPlot a = vipsOp (Lookup :: Nickname "hist_plot") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |histogram equalisation
histEqual :: GV.Image ->  HistEqual
histEqual a = vipsOp (Lookup :: Nickname "hist_equal") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |normalise histogram
histNorm :: GV.Image ->  HistNorm
histNorm a = vipsOp (Lookup :: Nickname "hist_norm") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |match two histograms
histMatch :: GV.Image -> GV.Image ->  HistMatch
histMatch a b = vipsOp (Lookup :: Nickname "hist_match") & inputs & outputs
  where
    inputs = V.ref a . V.img b
    outputs = V.outImg

-- |form cumulative histogram
histCum :: GV.Image ->  HistCum
histCum a = vipsOp (Lookup :: Nickname "hist_cum") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |statistical difference
stdif :: Int32 -> Int32 -> GV.Image ->  Stdif
stdif a b c = vipsOp (Lookup :: Nickname "stdif") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.img c
    outputs = V.outImg

-- |find threshold for percent of pixels
percent :: Double -> GV.Image ->  Percent
percent a b = vipsOp (Lookup :: Nickname "percent") & inputs & outputs
  where
    inputs = V.percent a . V.img b
    outputs = V.outThreshold

-- |use pixel values to pick cases from an array of images
vipsCase :: GV.ArrayImage -> GV.Image ->  Case
vipsCase a b = vipsOp (Lookup :: Nickname "case") & inputs & outputs
  where
    inputs = V.cases a . V.index b
    outputs = V.outImg

-- |map an image though a lut
maplut :: GV.Image -> GV.Image ->  Maplut
maplut a b = vipsOp (Lookup :: Nickname "maplut") & inputs & outputs
  where
    inputs = V.lut a . V.img b
    outputs = V.outImg

-- |load named ICC profile
profileLoad :: T.Text ->  ProfileLoad
profileLoad a = vipsOp (Lookup :: Nickname "profile_load") & inputs & outputs
  where
    inputs = V.name a
    outputs = V.outProfile

-- |transform XYZ to CMYK
xyZ2Cmyk :: GV.Image ->  XyZ2Cmyk
xyZ2Cmyk a = vipsOp (Lookup :: Nickname "XYZ2CMYK") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform CMYK to XYZ
cmyK2Xyz :: GV.Image ->  CmyK2Xyz
cmyK2Xyz a = vipsOp (Lookup :: Nickname "CMYK2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |convert an scRGB image to sRGB
scRgB2sRgb :: GV.Image ->  ScRgB2sRgb
scRgB2sRgb a = vipsOp (Lookup :: Nickname "scRGB2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |convert scRGB to BW
scRgB2Bw :: GV.Image ->  ScRgB2Bw
scRgB2Bw a = vipsOp (Lookup :: Nickname "scRGB2BW") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |convert an sRGB image to scRGB
sRgB2scRgb :: GV.Image ->  SRgB2scRgb
sRgB2scRgb a = vipsOp (Lookup :: Nickname "sRGB2scRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |calculate dECMC
dEcmc :: GV.Image -> GV.Image ->  DEcmc
dEcmc a b = vipsOp (Lookup :: Nickname "dECMC") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |calculate dE00
dE00 :: GV.Image -> GV.Image ->  DE00
dE00 a b = vipsOp (Lookup :: Nickname "dE00") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |calculate dE76
dE76 :: GV.Image -> GV.Image ->  DE76
dE76 a b = vipsOp (Lookup :: Nickname "dE76") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |transform between devices with ICC profiles
iccTransform :: T.Text -> GV.Image ->  IccTransform
iccTransform a b = vipsOp (Lookup :: Nickname "icc_transform") & inputs & outputs
  where
    inputs = V.outputProfile a . V.img b
    outputs = V.outImg

-- |output to device with ICC profile
iccExport :: GV.Image ->  IccExport
iccExport a = vipsOp (Lookup :: Nickname "icc_export") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |import from device with ICC profile
iccImport :: GV.Image ->  IccImport
iccImport a = vipsOp (Lookup :: Nickname "icc_import") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform HSV to sRGB
hsV2sRgb :: GV.Image ->  HsV2sRgb
hsV2sRgb a = vipsOp (Lookup :: Nickname "HSV2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform sRGB to HSV
sRgB2Hsv :: GV.Image ->  SRgB2Hsv
sRgB2Hsv a = vipsOp (Lookup :: Nickname "sRGB2HSV") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |convert a LabQ image to sRGB
labQ2sRgb :: GV.Image ->  LabQ2sRgb
labQ2sRgb a = vipsOp (Lookup :: Nickname "LabQ2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform float RGB to Radiance coding
float2rad :: GV.Image ->  Float2rad
float2rad a = vipsOp (Lookup :: Nickname "float2rad") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |unpack Radiance coding to float RGB
rad2float :: GV.Image ->  Rad2float
rad2float a = vipsOp (Lookup :: Nickname "rad2float") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform float Lab to signed short
lab2LabS :: GV.Image ->  Lab2LabS
lab2LabS a = vipsOp (Lookup :: Nickname "Lab2LabS") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform signed short Lab to float
labS2Lab :: GV.Image ->  LabS2Lab
labS2Lab a = vipsOp (Lookup :: Nickname "LabS2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform short Lab to LabQ coding
labS2LabQ :: GV.Image ->  LabS2LabQ
labS2LabQ a = vipsOp (Lookup :: Nickname "LabS2LabQ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |unpack a LabQ image to short Lab
labQ2LabS :: GV.Image ->  LabQ2LabS
labQ2LabS a = vipsOp (Lookup :: Nickname "LabQ2LabS") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform float Lab to LabQ coding
lab2LabQ :: GV.Image ->  Lab2LabQ
lab2LabQ a = vipsOp (Lookup :: Nickname "Lab2LabQ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |unpack a LabQ image to float Lab
labQ2Lab :: GV.Image ->  LabQ2Lab
labQ2Lab a = vipsOp (Lookup :: Nickname "LabQ2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform XYZ to scRGB
xyZ2scRgb :: GV.Image ->  XyZ2scRgb
xyZ2scRgb a = vipsOp (Lookup :: Nickname "XYZ2scRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform scRGB to XYZ
scRgB2Xyz :: GV.Image ->  ScRgB2Xyz
scRgB2Xyz a = vipsOp (Lookup :: Nickname "scRGB2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform Yxy to XYZ
yxy2Xyz :: GV.Image ->  Yxy2Xyz
yxy2Xyz a = vipsOp (Lookup :: Nickname "Yxy2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform XYZ to Yxy
xyZ2Yxy :: GV.Image ->  XyZ2Yxy
xyZ2Yxy a = vipsOp (Lookup :: Nickname "XYZ2Yxy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform LCh to CMC
cmC2LCh :: GV.Image ->  CmC2LCh
cmC2LCh a = vipsOp (Lookup :: Nickname "CMC2LCh") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform LCh to CMC
lCh2Cmc :: GV.Image ->  LCh2Cmc
lCh2Cmc a = vipsOp (Lookup :: Nickname "LCh2CMC") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform LCh to Lab
lCh2Lab :: GV.Image ->  LCh2Lab
lCh2Lab a = vipsOp (Lookup :: Nickname "LCh2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform Lab to LCh
lab2LCh :: GV.Image ->  Lab2LCh
lab2LCh a = vipsOp (Lookup :: Nickname "Lab2LCh") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform XYZ to Lab
xyZ2Lab :: GV.Image ->  XyZ2Lab
xyZ2Lab a = vipsOp (Lookup :: Nickname "XYZ2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transform CIELAB to XYZ
lab2Xyz :: GV.Image ->  Lab2Xyz
lab2Xyz a = vipsOp (Lookup :: Nickname "Lab2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |convert to a new colorspace
colourspace :: GV.Interpretation -> GV.Image ->  Colourspace
colourspace a b = vipsOp (Lookup :: Nickname "colourspace") & inputs & outputs
  where
    inputs = V.space a . V.img b
    outputs = V.outImg

-- |resize an image
resize :: Double -> GV.Image ->  Resize
resize a b = vipsOp (Lookup :: Nickname "resize") & inputs & outputs
  where
    inputs = V.scale a . V.img b
    outputs = V.outImg

-- |rotate an image by a number of degrees
rotate :: Double -> GV.Image ->  Rotate
rotate a b = vipsOp (Lookup :: Nickname "rotate") & inputs & outputs
  where
    inputs = V.angle a . V.img b
    outputs = V.outImg

-- |similarity transform of an image
similarity :: GV.Image ->  Similarity
similarity a = vipsOp (Lookup :: Nickname "similarity") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |affine transform of an image
affine :: GV.ArrayDouble -> GV.Image ->  Affine
affine a b = vipsOp (Lookup :: Nickname "affine") & inputs & outputs
  where
    inputs = V.matrix a . V.img b
    outputs = V.outImg

-- |resample an image with a quadratic transform
quadratic :: GV.Image -> GV.Image ->  Quadratic
quadratic a b = vipsOp (Lookup :: Nickname "quadratic") & inputs & outputs
  where
    inputs = V.coeff a . V.img b
    outputs = V.outImg

-- |reduce an image
reduce :: Double -> Double -> GV.Image ->  Reduce
reduce a b c = vipsOp (Lookup :: Nickname "reduce") & inputs & outputs
  where
    inputs = V.vshrink a . V.hshrink b . V.img c
    outputs = V.outImg

-- |shrink an image vertically
reducev :: Double -> GV.Image ->  Reducev
reducev a b = vipsOp (Lookup :: Nickname "reducev") & inputs & outputs
  where
    inputs = V.vshrink a . V.img b
    outputs = V.outImg

-- |shrink an image horizontally
reduceh :: Double -> GV.Image ->  Reduceh
reduceh a b = vipsOp (Lookup :: Nickname "reduceh") & inputs & outputs
  where
    inputs = V.hshrink a . V.img b
    outputs = V.outImg

-- |shrink an image vertically
shrinkv :: Int32 -> GV.Image ->  Shrinkv
shrinkv a b = vipsOp (Lookup :: Nickname "shrinkv") & inputs & outputs
  where
    inputs = V.vshrink a . V.img b
    outputs = V.outImg

-- |shrink an image horizontally
shrinkh :: Int32 -> GV.Image ->  Shrinkh
shrinkh a b = vipsOp (Lookup :: Nickname "shrinkh") & inputs & outputs
  where
    inputs = V.hshrink a . V.img b
    outputs = V.outImg

-- |shrink an image
shrink :: Double -> Double -> GV.Image ->  Shrink
shrink a b c = vipsOp (Lookup :: Nickname "shrink") & inputs & outputs
  where
    inputs = V.vshrink a . V.hshrink b . V.img c
    outputs = V.outImg

-- |resample with a map image
mapim :: GV.Image -> GV.Image ->  Mapim
mapim a b = vipsOp (Lookup :: Nickname "mapim") & inputs & outputs
  where
    inputs = V.index a . V.img b
    outputs = V.outImg

-- |generate thumbnail from source
thumbnailSource :: Int32 -> VipsSource ->  ThumbnailSource
thumbnailSource a b = vipsOp (Lookup :: Nickname "thumbnail_source") & inputs & outputs
  where
    inputs = V.width a . V.source b
    outputs = V.outImg

-- |generate thumbnail from image
thumbnailImage :: Int32 -> GV.Image ->  ThumbnailImage
thumbnailImage a b = vipsOp (Lookup :: Nickname "thumbnail_image") & inputs & outputs
  where
    inputs = V.width a . V.img b
    outputs = V.outImg

-- |generate thumbnail from buffer
thumbnailBuffer :: Int32 -> GV.Blob ->  ThumbnailBuffer
thumbnailBuffer a b = vipsOp (Lookup :: Nickname "thumbnail_buffer") & inputs & outputs
  where
    inputs = V.width a . V.buffer b
    outputs = V.outImg

-- |generate thumbnail from file
thumbnail :: Int32 -> T.Text ->  Thumbnail
thumbnail a b = vipsOp (Lookup :: Nickname "thumbnail") & inputs & outputs
  where
    inputs = V.width a . V.filename b
    outputs = V.outImg

-- |save image in HEIF format
heifsaveTarget :: VipsTarget -> GV.Image ->  HeifsaveTarget
heifsaveTarget a b = vipsOp (Lookup :: Nickname "heifsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image in HEIF format
heifsaveBuffer :: GV.Image ->  HeifsaveBuffer
heifsaveBuffer a = vipsOp (Lookup :: Nickname "heifsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image in HEIF format
heifsave :: T.Text -> GV.Image ->  Heifsave
heifsave a b = vipsOp (Lookup :: Nickname "heifsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to fits file
fitssave :: T.Text -> GV.Image ->  Fitssave
fitssave a b = vipsOp (Lookup :: Nickname "fitssave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to magick buffer
magicksaveBuffer :: GV.Image ->  MagicksaveBuffer
magicksaveBuffer a = vipsOp (Lookup :: Nickname "magicksave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save file with ImageMagick
magicksave :: T.Text -> GV.Image ->  Magicksave
magicksave a b = vipsOp (Lookup :: Nickname "magicksave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to tiff buffer
tiffsaveBuffer :: GV.Image ->  TiffsaveBuffer
tiffsaveBuffer a = vipsOp (Lookup :: Nickname "tiffsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to tiff file
tiffsave :: T.Text -> GV.Image ->  Tiffsave
tiffsave a b = vipsOp (Lookup :: Nickname "tiffsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to webp target
webpsaveTarget :: VipsTarget -> GV.Image ->  WebpsaveTarget
webpsaveTarget a b = vipsOp (Lookup :: Nickname "webpsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to webp buffer
webpsaveBuffer :: GV.Image ->  WebpsaveBuffer
webpsaveBuffer a = vipsOp (Lookup :: Nickname "webpsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to webp file
webpsave :: T.Text -> GV.Image ->  Webpsave
webpsave a b = vipsOp (Lookup :: Nickname "webpsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to jpeg mime
jpegsaveMime :: GV.Image ->  JpegsaveMime
jpegsaveMime a = vipsOp (Lookup :: Nickname "jpegsave_mime") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.void

-- |save image to jpeg target
jpegsaveTarget :: VipsTarget -> GV.Image ->  JpegsaveTarget
jpegsaveTarget a b = vipsOp (Lookup :: Nickname "jpegsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to jpeg buffer
jpegsaveBuffer :: GV.Image ->  JpegsaveBuffer
jpegsaveBuffer a = vipsOp (Lookup :: Nickname "jpegsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to jpeg file
jpegsave :: T.Text -> GV.Image ->  Jpegsave
jpegsave a b = vipsOp (Lookup :: Nickname "jpegsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to target as PNG
pngsaveTarget :: VipsTarget -> GV.Image ->  PngsaveTarget
pngsaveTarget a b = vipsOp (Lookup :: Nickname "pngsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to png buffer
pngsaveBuffer :: GV.Image ->  PngsaveBuffer
pngsaveBuffer a = vipsOp (Lookup :: Nickname "pngsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to png file
pngsave :: T.Text -> GV.Image ->  Pngsave
pngsave a b = vipsOp (Lookup :: Nickname "pngsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to dz buffer
dzsaveBuffer :: GV.Image ->  DzsaveBuffer
dzsaveBuffer a = vipsOp (Lookup :: Nickname "dzsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to deepzoom file
dzsave :: T.Text -> GV.Image ->  Dzsave
dzsave a b = vipsOp (Lookup :: Nickname "dzsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to Radiance target
radsaveTarget :: VipsTarget -> GV.Image ->  RadsaveTarget
radsaveTarget a b = vipsOp (Lookup :: Nickname "radsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to Radiance buffer
radsaveBuffer :: GV.Image ->  RadsaveBuffer
radsaveBuffer a = vipsOp (Lookup :: Nickname "radsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

-- |save image to Radiance file
radsave :: T.Text -> GV.Image ->  Radsave
radsave a b = vipsOp (Lookup :: Nickname "radsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save to ppm
ppmsaveTarget :: VipsTarget -> GV.Image ->  PpmsaveTarget
ppmsaveTarget a b = vipsOp (Lookup :: Nickname "ppmsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to ppm file
ppmsave :: T.Text -> GV.Image ->  Ppmsave
ppmsave a b = vipsOp (Lookup :: Nickname "ppmsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to vips file
vipssave :: T.Text -> GV.Image ->  Vipssave
vipssave a b = vipsOp (Lookup :: Nickname "vipssave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |write raw image to file descriptor
rawsaveFd :: Int32 -> GV.Image ->  RawsaveFd
rawsaveFd a b = vipsOp (Lookup :: Nickname "rawsave_fd") & inputs & outputs
  where
    inputs = V.fd a . V.img b
    outputs = V.void

-- |save image to raw file
rawsave :: T.Text -> GV.Image ->  Rawsave
rawsave a b = vipsOp (Lookup :: Nickname "rawsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |print matrix
matrixprint :: GV.Image ->  Matrixprint
matrixprint a = vipsOp (Lookup :: Nickname "matrixprint") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.void

-- |save image to matrix
matrixsaveTarget :: VipsTarget -> GV.Image ->  MatrixsaveTarget
matrixsaveTarget a b = vipsOp (Lookup :: Nickname "matrixsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to matrix
matrixsave :: T.Text -> GV.Image ->  Matrixsave
matrixsave a b = vipsOp (Lookup :: Nickname "matrixsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |save image to csv
csvsaveTarget :: VipsTarget -> GV.Image ->  CsvsaveTarget
csvsaveTarget a b = vipsOp (Lookup :: Nickname "csvsave_target") & inputs & outputs
  where
    inputs = V.target a . V.img b
    outputs = V.void

-- |save image to csv
csvsave :: T.Text -> GV.Image ->  Csvsave
csvsave a b = vipsOp (Lookup :: Nickname "csvsave") & inputs & outputs
  where
    inputs = V.filename a . V.img b
    outputs = V.void

-- |load a HEIF image
heifloadSource :: VipsSource ->  HeifloadSource
heifloadSource a = vipsOp (Lookup :: Nickname "heifload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outHeifloadSourceResult

-- |load a HEIF image
heifloadBuffer :: GV.Blob ->  HeifloadBuffer
heifloadBuffer a = vipsOp (Lookup :: Nickname "heifload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outHeifloadBufferResult

-- |load a HEIF image
heifload :: T.Text ->  Heifload
heifload a = vipsOp (Lookup :: Nickname "heifload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outHeifloadResult

-- |load an OpenEXR image
openexrload :: T.Text ->  Openexrload
openexrload a = vipsOp (Lookup :: Nickname "openexrload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outOpenexrloadResult

-- |load a FITS image
fitsload :: T.Text ->  Fitsload
fitsload a = vipsOp (Lookup :: Nickname "fitsload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outFitsloadResult

-- |load buffer with ImageMagick7
magickloadBuffer :: GV.Blob ->  MagickloadBuffer
magickloadBuffer a = vipsOp (Lookup :: Nickname "magickload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outMagickloadBufferResult

-- |load file with ImageMagick7
magickload :: T.Text ->  Magickload
magickload a = vipsOp (Lookup :: Nickname "magickload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outMagickloadResult

-- |load tiff from source
tiffloadSource :: VipsSource ->  TiffloadSource
tiffloadSource a = vipsOp (Lookup :: Nickname "tiffload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outTiffloadSourceResult

-- |load tiff from buffer
tiffloadBuffer :: GV.Blob ->  TiffloadBuffer
tiffloadBuffer a = vipsOp (Lookup :: Nickname "tiffload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outTiffloadBufferResult

-- |load tiff from file
tiffload :: T.Text ->  Tiffload
tiffload a = vipsOp (Lookup :: Nickname "tiffload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outTiffloadResult

-- |load webp from source
webploadSource :: VipsSource ->  WebploadSource
webploadSource a = vipsOp (Lookup :: Nickname "webpload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outWebploadSourceResult

-- |load webp from buffer
webploadBuffer :: GV.Blob ->  WebploadBuffer
webploadBuffer a = vipsOp (Lookup :: Nickname "webpload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outWebploadBufferResult

-- |load webp from file
webpload :: T.Text ->  Webpload
webpload a = vipsOp (Lookup :: Nickname "webpload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outWebploadResult

-- |load image from jpeg source
jpegloadSource :: VipsSource ->  JpegloadSource
jpegloadSource a = vipsOp (Lookup :: Nickname "jpegload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outJpegloadSourceResult

-- |load jpeg from buffer
jpegloadBuffer :: GV.Blob ->  JpegloadBuffer
jpegloadBuffer a = vipsOp (Lookup :: Nickname "jpegload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outJpegloadBufferResult

-- |load jpeg from file
jpegload :: T.Text ->  Jpegload
jpegload a = vipsOp (Lookup :: Nickname "jpegload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outJpegloadResult

-- |load png from source
pngloadSource :: VipsSource ->  PngloadSource
pngloadSource a = vipsOp (Lookup :: Nickname "pngload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outPngloadSourceResult

-- |load png from buffer
pngloadBuffer :: GV.Blob ->  PngloadBuffer
pngloadBuffer a = vipsOp (Lookup :: Nickname "pngload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outPngloadBufferResult

-- |load png from file
pngload :: T.Text ->  Pngload
pngload a = vipsOp (Lookup :: Nickname "pngload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outPngloadResult

-- |load GIF with giflib
gifloadSource :: VipsSource ->  GifloadSource
gifloadSource a = vipsOp (Lookup :: Nickname "gifload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outGifloadSourceResult

-- |load GIF with giflib
gifloadBuffer :: GV.Blob ->  GifloadBuffer
gifloadBuffer a = vipsOp (Lookup :: Nickname "gifload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outGifloadBufferResult

-- |load GIF with giflib
gifload :: T.Text ->  Gifload
gifload a = vipsOp (Lookup :: Nickname "gifload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outGifloadResult

-- |load svg from source
svgloadSource :: VipsSource ->  SvgloadSource
svgloadSource a = vipsOp (Lookup :: Nickname "svgload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outSvgloadSourceResult

-- |load SVG with rsvg
svgloadBuffer :: GV.Blob ->  SvgloadBuffer
svgloadBuffer a = vipsOp (Lookup :: Nickname "svgload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outSvgloadBufferResult

-- |load SVG with rsvg
svgload :: T.Text ->  Svgload
svgload a = vipsOp (Lookup :: Nickname "svgload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outSvgloadResult

-- |load PDF from source
pdfloadSource :: VipsSource ->  PdfloadSource
pdfloadSource a = vipsOp (Lookup :: Nickname "pdfload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outPdfloadSourceResult

-- |load PDF from buffer
pdfloadBuffer :: GV.Blob ->  PdfloadBuffer
pdfloadBuffer a = vipsOp (Lookup :: Nickname "pdfload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outPdfloadBufferResult

-- |load PDF from file
pdfload :: T.Text ->  Pdfload
pdfload a = vipsOp (Lookup :: Nickname "pdfload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outPdfloadResult

-- |load rad from source
radloadSource :: VipsSource ->  RadloadSource
radloadSource a = vipsOp (Lookup :: Nickname "radload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outRadloadSourceResult

-- |load rad from buffer
radloadBuffer :: GV.Blob ->  RadloadBuffer
radloadBuffer a = vipsOp (Lookup :: Nickname "radload_buffer") & inputs & outputs
  where
    inputs = V.buffer a
    outputs = V.outRadloadBufferResult

-- |load a Radiance image from a file
radload :: T.Text ->  Radload
radload a = vipsOp (Lookup :: Nickname "radload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outRadloadResult

-- |load ppm base class
ppmloadSource :: VipsSource ->  PpmloadSource
ppmloadSource a = vipsOp (Lookup :: Nickname "ppmload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outPpmloadSourceResult

-- |load ppm from file
ppmload :: T.Text ->  Ppmload
ppmload a = vipsOp (Lookup :: Nickname "ppmload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outPpmloadResult

-- |load an Analyze6 image
analyzeload :: T.Text ->  Analyzeload
analyzeload a = vipsOp (Lookup :: Nickname "analyzeload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outAnalyzeloadResult

-- |load vips from file
vipsload :: T.Text ->  Vipsload
vipsload a = vipsOp (Lookup :: Nickname "vipsload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outVipsloadResult

-- |load raw data from a file
rawload :: Int32 -> Int32 -> Int32 -> T.Text ->  Rawload
rawload a b c d = vipsOp (Lookup :: Nickname "rawload") & inputs & outputs
  where
    inputs = V.bands a . V.height b . V.width c . V.filename d
    outputs = V.outRawloadResult

-- |load matrix
matrixloadSource :: VipsSource ->  MatrixloadSource
matrixloadSource a = vipsOp (Lookup :: Nickname "matrixload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outMatrixloadSourceResult

-- |load matrix
matrixload :: T.Text ->  Matrixload
matrixload a = vipsOp (Lookup :: Nickname "matrixload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outMatrixloadResult

-- |load csv
csvloadSource :: VipsSource ->  CsvloadSource
csvloadSource a = vipsOp (Lookup :: Nickname "csvload_source") & inputs & outputs
  where
    inputs = V.source a
    outputs = V.outCsvloadSourceResult

-- |load csv
csvload :: T.Text ->  Csvload
csvload a = vipsOp (Lookup :: Nickname "csvload") & inputs & outputs
  where
    inputs = V.filename a
    outputs = V.outCsvloadResult

-- |find the index of the first non-zero pixel in tests
switch :: GV.ArrayImage ->  Switch
switch a = vipsOp (Lookup :: Nickname "switch") & inputs & outputs
  where
    inputs = V.tests a
    outputs = V.outImg

-- |make a perlin noise image
perlin :: Int32 -> Int32 ->  Perlin
perlin a b = vipsOp (Lookup :: Nickname "perlin") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a worley noise image
worley :: Int32 -> Int32 ->  Worley
worley a b = vipsOp (Lookup :: Nickname "worley") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a fractal surface
fractsurf :: Double -> Int32 -> Int32 ->  Fractsurf
fractsurf a b c = vipsOp (Lookup :: Nickname "fractsurf") & inputs & outputs
  where
    inputs = V.fractalDimension a . V.height b . V.width c
    outputs = V.outImg

-- |make a 1D image where pixel values are indexes
identity ::  Identity
identity  = vipsOp (Lookup :: Nickname "identity") & inputs & outputs
  where
    inputs = id
    outputs = V.outImg

-- |build a look-up table
tonelut ::  Tonelut
tonelut  = vipsOp (Lookup :: Nickname "tonelut") & inputs & outputs
  where
    inputs = id
    outputs = V.outImg

-- |build an inverted look-up table
invertlut :: GV.Image ->  Invertlut
invertlut a = vipsOp (Lookup :: Nickname "invertlut") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |build a look-up table
buildlut :: GV.Image ->  Buildlut
buildlut a = vipsOp (Lookup :: Nickname "buildlut") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |make fractal filter
maskFractal :: Double -> Int32 -> Int32 ->  MaskFractal
maskFractal a b c = vipsOp (Lookup :: Nickname "mask_fractal") & inputs & outputs
  where
    inputs = V.fractalDimension a . V.height b . V.width c
    outputs = V.outImg

-- |make a gaussian filter
maskGaussianBand :: Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskGaussianBand
maskGaussianBand a b c d e f = vipsOp (Lookup :: Nickname "mask_gaussian_band") & inputs & outputs
  where
    inputs = V.amplitudeCutoff a . V.radius b . V.frequencyCutoffY c . V.frequencyCutoffX d . V.height e . V.width f
    outputs = V.outImg

-- |make a gaussian ring filter
maskGaussianRing :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskGaussianRing
maskGaussianRing a b c d e = vipsOp (Lookup :: Nickname "mask_gaussian_ring") & inputs & outputs
  where
    inputs = V.ringwidth a . V.amplitudeCutoff b . V.frequencyCutoff c . V.height d . V.width e
    outputs = V.outImg

-- |make a gaussian filter
maskGaussian :: Double -> Double -> Int32 -> Int32 ->  MaskGaussian
maskGaussian a b c d = vipsOp (Lookup :: Nickname "mask_gaussian") & inputs & outputs
  where
    inputs = V.amplitudeCutoff a . V.frequencyCutoff b . V.height c . V.width d
    outputs = V.outImg

-- |make a butterworth_band filter
maskButterworthBand :: Double -> Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworthBand
maskButterworthBand a b c d e f g = vipsOp (Lookup :: Nickname "mask_butterworth_band") & inputs & outputs
  where
    inputs = V.amplitudeCutoff a . V.radius b . V.frequencyCutoffY c . V.frequencyCutoffX d . V.order e . V.height f . V.width g
    outputs = V.outImg

-- |make a butterworth ring filter
maskButterworthRing :: Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworthRing
maskButterworthRing a b c d e f = vipsOp (Lookup :: Nickname "mask_butterworth_ring") & inputs & outputs
  where
    inputs = V.ringwidth a . V.amplitudeCutoff b . V.frequencyCutoff c . V.order d . V.height e . V.width f
    outputs = V.outImg

-- |make a butterworth filter
maskButterworth :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworth
maskButterworth a b c d e = vipsOp (Lookup :: Nickname "mask_butterworth") & inputs & outputs
  where
    inputs = V.amplitudeCutoff a . V.frequencyCutoff b . V.order c . V.height d . V.width e
    outputs = V.outImg

-- |make an ideal band filter
maskIdealBand :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskIdealBand
maskIdealBand a b c d e = vipsOp (Lookup :: Nickname "mask_ideal_band") & inputs & outputs
  where
    inputs = V.radius a . V.frequencyCutoffY b . V.frequencyCutoffX c . V.height d . V.width e
    outputs = V.outImg

-- |make an ideal ring filter
maskIdealRing :: Double -> Double -> Int32 -> Int32 ->  MaskIdealRing
maskIdealRing a b c d = vipsOp (Lookup :: Nickname "mask_ideal_ring") & inputs & outputs
  where
    inputs = V.ringwidth a . V.frequencyCutoff b . V.height c . V.width d
    outputs = V.outImg

-- |make an ideal filter
maskIdeal :: Double -> Int32 -> Int32 ->  MaskIdeal
maskIdeal a b c = vipsOp (Lookup :: Nickname "mask_ideal") & inputs & outputs
  where
    inputs = V.frequencyCutoff a . V.height b . V.width c
    outputs = V.outImg

-- |make a 2D sine wave
sines :: Int32 -> Int32 ->  Sines
sines a b = vipsOp (Lookup :: Nickname "sines") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a zone plate
zone :: Int32 -> Int32 ->  Zone
zone a b = vipsOp (Lookup :: Nickname "zone") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a grey ramp image
grey :: Int32 -> Int32 ->  Grey
grey a b = vipsOp (Lookup :: Nickname "grey") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make an image showing the eye's spatial response
eye :: Int32 -> Int32 ->  Eye
eye a b = vipsOp (Lookup :: Nickname "eye") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a laplacian of gaussian image
logmat :: Double -> Double ->  Logmat
logmat a b = vipsOp (Lookup :: Nickname "logmat") & inputs & outputs
  where
    inputs = V.minAmpl a . V.sigma b
    outputs = V.outImg

-- |make a gaussian image
gaussmat :: Double -> Double ->  Gaussmat
gaussmat a b = vipsOp (Lookup :: Nickname "gaussmat") & inputs & outputs
  where
    inputs = V.minAmpl a . V.sigma b
    outputs = V.outImg

-- |make an image where pixel values are coordinates
xyz :: Int32 -> Int32 ->  Xyz
xyz a b = vipsOp (Lookup :: Nickname "xyz") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a text image
text :: T.Text ->  Text
text a = vipsOp (Lookup :: Nickname "text") & inputs & outputs
  where
    inputs = V.text a
    outputs = V.outTextResult

-- |make a gaussnoise image
gaussnoise :: Int32 -> Int32 ->  Gaussnoise
gaussnoise a b = vipsOp (Lookup :: Nickname "gaussnoise") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |make a black image
black :: Int32 -> Int32 ->  Black
black a b = vipsOp (Lookup :: Nickname "black") & inputs & outputs
  where
    inputs = V.height a . V.width b
    outputs = V.outImg

-- |blend a pair of images with a blend mode
composite2 :: GV.BlendMode -> GV.Image -> GV.Image ->  Composite2
composite2 a b c = vipsOp (Lookup :: Nickname "composite2") & inputs & outputs
  where
    inputs = V.mode a . V.overlay b . V.base c
    outputs = V.outImg

-- |blend an array of images with an array of blend modes
composite :: GV.ArrayInt -> GV.ArrayImage ->  Composite
composite a b = vipsOp (Lookup :: Nickname "composite") & inputs & outputs
  where
    inputs = V.mode a . V.imgs b
    outputs = V.outImg

-- |gamma an image
gamma :: GV.Image ->  Gamma
gamma a = vipsOp (Lookup :: Nickname "gamma") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |false-color an image
falsecolour :: GV.Image ->  Falsecolour
falsecolour a = vipsOp (Lookup :: Nickname "falsecolour") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |byteswap an image
byteswap :: GV.Image ->  Byteswap
byteswap a = vipsOp (Lookup :: Nickname "byteswap") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |pick most-significant byte from an image
msb :: GV.Image ->  Msb
msb a = vipsOp (Lookup :: Nickname "msb") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |subsample an image
subsample :: Int32 -> Int32 -> GV.Image ->  Subsample
subsample a b c = vipsOp (Lookup :: Nickname "subsample") & inputs & outputs
  where
    inputs = V.yfac a . V.xfac b . V.input c
    outputs = V.outImg

-- |zoom an image
zoom :: Int32 -> Int32 -> GV.Image ->  Zoom
zoom a b c = vipsOp (Lookup :: Nickname "zoom") & inputs & outputs
  where
    inputs = V.yfac a . V.xfac b . V.input c
    outputs = V.outImg

-- |wrap image origin
wrap :: GV.Image ->  Wrap
wrap a = vipsOp (Lookup :: Nickname "wrap") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |scale an image to uchar
scale :: GV.Image ->  Scale
scale a = vipsOp (Lookup :: Nickname "scale") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |transpose3d an image
transpose3d :: GV.Image ->  Transpose3d
transpose3d a = vipsOp (Lookup :: Nickname "transpose3d") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |grid an image
grid :: Int32 -> Int32 -> Int32 -> GV.Image ->  Grid
grid a b c d = vipsOp (Lookup :: Nickname "grid") & inputs & outputs
  where
    inputs = V.down a . V.across b . V.tileHeight c . V.img d
    outputs = V.outImg

-- |unpremultiply image alpha
unpremultiply :: GV.Image ->  Unpremultiply
unpremultiply a = vipsOp (Lookup :: Nickname "unpremultiply") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |premultiply image alpha
premultiply :: GV.Image ->  Premultiply
premultiply a = vipsOp (Lookup :: Nickname "premultiply") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |flatten alpha out of an image
flatten :: GV.Image ->  Flatten
flatten a = vipsOp (Lookup :: Nickname "flatten") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |unfold image bands into x axis
bandunfold :: GV.Image ->  Bandunfold
bandunfold a = vipsOp (Lookup :: Nickname "bandunfold") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |fold up x axis into bands
bandfold :: GV.Image ->  Bandfold
bandfold a = vipsOp (Lookup :: Nickname "bandfold") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |linear recombination with matrix
recomb :: GV.Image -> GV.Image ->  Recomb
recomb a b = vipsOp (Lookup :: Nickname "recomb") & inputs & outputs
  where
    inputs = V.m a . V.img b
    outputs = V.outImg

-- |ifthenelse an image
ifthenelse :: GV.Image -> GV.Image -> GV.Image ->  Ifthenelse
ifthenelse a b c = vipsOp (Lookup :: Nickname "ifthenelse") & inputs & outputs
  where
    inputs = V.in2 a . V.in1 b . V.cond c
    outputs = V.outImg

-- |autorotate image by exif tag
autorot :: GV.Image ->  Autorot
autorot a = vipsOp (Lookup :: Nickname "autorot") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outAutorotResult

-- |rotate an image
rot45 :: GV.Image ->  Rot45
rot45 a = vipsOp (Lookup :: Nickname "rot45") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |rotate an image
rot :: GV.Angle -> GV.Image ->  Rot
rot a b = vipsOp (Lookup :: Nickname "rot") & inputs & outputs
  where
    inputs = V.angle a . V.img b
    outputs = V.outImg

-- |cast an image
cast :: GV.BandFormat -> GV.Image ->  Cast
cast a b = vipsOp (Lookup :: Nickname "cast") & inputs & outputs
  where
    inputs = V.format a . V.img b
    outputs = V.outImg

-- |replicate an image
replicate :: Int32 -> Int32 -> GV.Image ->  Replicate
replicate a b c = vipsOp (Lookup :: Nickname "replicate") & inputs & outputs
  where
    inputs = V.down a . V.across b . V.img c
    outputs = V.outImg

-- |boolean operation across image bands
bandbool :: GV.OperationBoolean -> GV.Image ->  Bandbool
bandbool a b = vipsOp (Lookup :: Nickname "bandbool") & inputs & outputs
  where
    inputs = V.boolean a . V.img b
    outputs = V.outImg

-- |band-wise average
bandmean :: GV.Image ->  Bandmean
bandmean a = vipsOp (Lookup :: Nickname "bandmean") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |band-wise rank of a set of images
bandrank :: GV.ArrayImage ->  Bandrank
bandrank a = vipsOp (Lookup :: Nickname "bandrank") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

-- |append a constant band to an image
bandjoinConst :: GV.ArrayDouble -> GV.Image ->  BandjoinConst
bandjoinConst a b = vipsOp (Lookup :: Nickname "bandjoin_const") & inputs & outputs
  where
    inputs = V.c a . V.img b
    outputs = V.outImg

-- |bandwise join a set of images
bandjoin :: GV.ArrayImage ->  Bandjoin
bandjoin a = vipsOp (Lookup :: Nickname "bandjoin") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

-- |extract band from an image
extractBand :: Int32 -> GV.Image ->  ExtractBand
extractBand a b = vipsOp (Lookup :: Nickname "extract_band") & inputs & outputs
  where
    inputs = V.band a . V.img b
    outputs = V.outImg

-- |extract an area from an image
smartcrop :: Int32 -> Int32 -> GV.Image ->  Smartcrop
smartcrop a b c = vipsOp (Lookup :: Nickname "smartcrop") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.input c
    outputs = V.outImg

-- |extract an area from an image
extractArea :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  ExtractArea
extractArea a b c d e = vipsOp (Lookup :: Nickname "extract_area") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.top c . V.left d . V.input e
    outputs = V.outImg

-- |join an array of images
arrayjoin :: GV.ArrayImage ->  Arrayjoin
arrayjoin a = vipsOp (Lookup :: Nickname "arrayjoin") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

-- |join a pair of images
join :: GV.Direction -> GV.Image -> GV.Image ->  Join
join a b c = vipsOp (Lookup :: Nickname "join") & inputs & outputs
  where
    inputs = V.direction a . V.in2 b . V.in1 c
    outputs = V.outImg

-- |insert image @sub into @main at @x, @y
insert :: Int32 -> Int32 -> GV.Image -> GV.Image ->  Insert
insert a b c d = vipsOp (Lookup :: Nickname "insert") & inputs & outputs
  where
    inputs = V.y a . V.x b . V.sub c . V.main d
    outputs = V.outImg

-- |flip an image
flip :: GV.Direction -> GV.Image ->  Flip
flip a b = vipsOp (Lookup :: Nickname "flip") & inputs & outputs
  where
    inputs = V.direction a . V.img b
    outputs = V.outImg

-- |place an image within a larger image with a certain gravity
gravity :: Int32 -> Int32 -> GV.CompassDirection -> GV.Image ->  Gravity
gravity a b c d = vipsOp (Lookup :: Nickname "gravity") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.direction c . V.img d
    outputs = V.outImg

-- |embed an image in a larger image
embed :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  Embed
embed a b c d e = vipsOp (Lookup :: Nickname "embed") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.y c . V.x d . V.img e
    outputs = V.outImg

-- |cache an image
cache :: GV.Image ->  Cache
cache a = vipsOp (Lookup :: Nickname "cache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |check sequential access
sequential :: GV.Image ->  Sequential
sequential a = vipsOp (Lookup :: Nickname "sequential") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |cache an image as a set of lines
linecache :: GV.Image ->  Linecache
linecache a = vipsOp (Lookup :: Nickname "linecache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |cache an image as a set of tiles
tilecache :: GV.Image ->  Tilecache
tilecache a = vipsOp (Lookup :: Nickname "tilecache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |copy an image
copy :: GV.Image ->  Copy
copy a = vipsOp (Lookup :: Nickname "copy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |search an image for non-edge areas
findTrim :: GV.Image ->  FindTrim
findTrim a = vipsOp (Lookup :: Nickname "find_trim") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outFindTrimResult

-- |read a point from an image
getpoint :: Int32 -> Int32 -> GV.Image ->  Getpoint
getpoint a b c = vipsOp (Lookup :: Nickname "getpoint") & inputs & outputs
  where
    inputs = V.y a . V.x b . V.img c
    outputs = V.outOutArray

-- |measure a set of patches on a color chart
measure :: Int32 -> Int32 -> GV.Image ->  Measure
measure a b c = vipsOp (Lookup :: Nickname "measure") & inputs & outputs
  where
    inputs = V.v a . V.h b . V.img c
    outputs = V.outImg

-- |find image profiles
profile :: GV.Image ->  Profile
profile a = vipsOp (Lookup :: Nickname "profile") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outProfileResult

-- |find image projections
project :: GV.Image ->  Project
project a = vipsOp (Lookup :: Nickname "project") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outProjectResult

-- |find hough circle transform
houghCircle :: GV.Image ->  HoughCircle
houghCircle a = vipsOp (Lookup :: Nickname "hough_circle") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |find hough line transform
houghLine :: GV.Image ->  HoughLine
houghLine a = vipsOp (Lookup :: Nickname "hough_line") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |find indexed image histogram
histFindIndexed :: GV.Image -> GV.Image ->  HistFindIndexed
histFindIndexed a b = vipsOp (Lookup :: Nickname "hist_find_indexed") & inputs & outputs
  where
    inputs = V.index a . V.img b
    outputs = V.outImg

-- |find n-dimensional image histogram
histFindNdim :: GV.Image ->  HistFindNdim
histFindNdim a = vipsOp (Lookup :: Nickname "hist_find_ndim") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |find image histogram
histFind :: GV.Image ->  HistFind
histFind a = vipsOp (Lookup :: Nickname "hist_find") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |find many image stats
stats :: GV.Image ->  Stats
stats a = vipsOp (Lookup :: Nickname "stats") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |find image standard deviation
deviate :: GV.Image ->  Deviate
deviate a = vipsOp (Lookup :: Nickname "deviate") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

-- |find image maximum
max :: GV.Image ->  Max
max a = vipsOp (Lookup :: Nickname "max") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMaxResult

-- |find image minimum
min :: GV.Image ->  Min
min a = vipsOp (Lookup :: Nickname "min") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMinResult

-- |find image average
avg :: GV.Image ->  Avg
avg a = vipsOp (Lookup :: Nickname "avg") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

-- |get a component from a complex image
complexget :: GV.OperationComplexget -> GV.Image ->  Complexget
complexget a b = vipsOp (Lookup :: Nickname "complexget") & inputs & outputs
  where
    inputs = V.get a . V.img b
    outputs = V.outImg

-- |perform a complex operation on an image
complex :: GV.OperationComplex -> GV.Image ->  Complex
complex a b = vipsOp (Lookup :: Nickname "complex") & inputs & outputs
  where
    inputs = V.cmplx a . V.img b
    outputs = V.outImg

-- |binary math operations with a constant
math2Const :: GV.ArrayDouble -> GV.OperationMath2 -> GV.Image ->  Math2Const
math2Const a b c = vipsOp (Lookup :: Nickname "math2_const") & inputs & outputs
  where
    inputs = V.c a . V.math2 b . V.img c
    outputs = V.outImg

-- |boolean operations against a constant
booleanConst :: GV.ArrayDouble -> GV.OperationBoolean -> GV.Image ->  BooleanConst
booleanConst a b c = vipsOp (Lookup :: Nickname "boolean_const") & inputs & outputs
  where
    inputs = V.c a . V.boolean b . V.img c
    outputs = V.outImg

-- |remainder after integer division of an image and a constant
remainderConst :: GV.ArrayDouble -> GV.Image ->  RemainderConst
remainderConst a b = vipsOp (Lookup :: Nickname "remainder_const") & inputs & outputs
  where
    inputs = V.c a . V.img b
    outputs = V.outImg

-- |relational operations against a constant
relationalConst :: GV.ArrayDouble -> GV.OperationRelational -> GV.Image ->  RelationalConst
relationalConst a b c = vipsOp (Lookup :: Nickname "relational_const") & inputs & outputs
  where
    inputs = V.c a . V.relational b . V.img c
    outputs = V.outImg

-- |perform a round function on an image
round :: GV.OperationRound -> GV.Image ->  Round
round a b = vipsOp (Lookup :: Nickname "round") & inputs & outputs
  where
    inputs = V.round a . V.img b
    outputs = V.outImg

-- |unit vector of pixel
sign :: GV.Image ->  Sign
sign a = vipsOp (Lookup :: Nickname "sign") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |absolute value of an image
abs :: GV.Image ->  Abs
abs a = vipsOp (Lookup :: Nickname "abs") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |apply a math operation to an image
math :: GV.OperationMath -> GV.Image ->  Math
math a b = vipsOp (Lookup :: Nickname "math") & inputs & outputs
  where
    inputs = V.math a . V.img b
    outputs = V.outImg

-- |calculate (a * in + b)
linear :: GV.ArrayDouble -> GV.ArrayDouble -> GV.Image ->  Linear
linear a b c = vipsOp (Lookup :: Nickname "linear") & inputs & outputs
  where
    inputs = V.b a . V.a b . V.img c
    outputs = V.outImg

-- |invert an image
invert :: GV.Image ->  Invert
invert a = vipsOp (Lookup :: Nickname "invert") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

-- |sum an array of images
sum :: GV.ArrayImage ->  Sum
sum a = vipsOp (Lookup :: Nickname "sum") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

-- |form a complex image from two real images
complexform :: GV.Image -> GV.Image ->  Complexform
complexform a b = vipsOp (Lookup :: Nickname "complexform") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |complex binary operations on two images
complex2 :: GV.OperationComplex2 -> GV.Image -> GV.Image ->  Complex2
complex2 a b c = vipsOp (Lookup :: Nickname "complex2") & inputs & outputs
  where
    inputs = V.cmplx a . V.right b . V.left c
    outputs = V.outImg

-- |binary math operations
math2 :: GV.OperationMath2 -> GV.Image -> GV.Image ->  Math2
math2 a b c = vipsOp (Lookup :: Nickname "math2") & inputs & outputs
  where
    inputs = V.math2 a . V.right b . V.left c
    outputs = V.outImg

-- |boolean operation on two images
boolean :: GV.OperationBoolean -> GV.Image -> GV.Image ->  Boolean
boolean a b c = vipsOp (Lookup :: Nickname "boolean") & inputs & outputs
  where
    inputs = V.boolean a . V.right b . V.left c
    outputs = V.outImg

-- |remainder after integer division of two images
remainder :: GV.Image -> GV.Image ->  Remainder
remainder a b = vipsOp (Lookup :: Nickname "remainder") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |relational operation on two images
relational :: GV.OperationRelational -> GV.Image -> GV.Image ->  Relational
relational a b c = vipsOp (Lookup :: Nickname "relational") & inputs & outputs
  where
    inputs = V.relational a . V.right b . V.left c
    outputs = V.outImg

-- |divide two images
divide :: GV.Image -> GV.Image ->  Divide
divide a b = vipsOp (Lookup :: Nickname "divide") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |multiply two images
multiply :: GV.Image -> GV.Image ->  Multiply
multiply a b = vipsOp (Lookup :: Nickname "multiply") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |subtract two images
subtract :: GV.Image -> GV.Image ->  Subtract
subtract a b = vipsOp (Lookup :: Nickname "subtract") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |add two images
add :: GV.Image -> GV.Image ->  Add
add a b = vipsOp (Lookup :: Nickname "add") & inputs & outputs
  where
    inputs = V.right a . V.left b
    outputs = V.outImg

-- |run an external command
system :: T.Text ->  System
system a = vipsOp (Lookup :: Nickname "system") & inputs & outputs
  where
    inputs = V.cmdFormat a
    outputs = V.outSystemResult

-- |extract an area from an image
crop :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  Crop
crop a b c d e = vipsOp (Lookup :: Nickname "crop") & inputs & outputs
  where
    inputs = V.height a . V.width b . V.top c . V.left d . V.input e
    outputs = V.outImg
