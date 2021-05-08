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
import           Data.Word
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
    inputs = V.filename' a'
    outputs = V.outImg

saveImage :: FilePath -> GV.Image -> SaveImage
saveImage a b = vipsForeignOp saver (Foreign :: Nickname "foreignSaveImage") & inputs & outputs
  where
    a' = T.pack a
    saver = GV.foreignFindSave a'
    inputs = V.filename' a' . V.img b
    outputs = V.void

--
-- Vips image operations:
--


globalbalance :: GV.Image ->  Globalbalance
globalbalance a = vipsOp (Lookup :: Nickname "globalbalance") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

match :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> GV.Image -> GV.Image ->  Match
match a b c d e f g h i j = vipsOp (Lookup :: Nickname "match") & inputs & outputs
  where
    inputs = V.ys2' a . V.xs2' b . V.yr2' c . V.xr2' d . V.ys1' e . V.xs1' f . V.yr1' g . V.xr1' h . V.sec' i . V.ref' j
    outputs = V.outImg

matrixinvert :: GV.Image ->  Matrixinvert
matrixinvert a = vipsOp (Lookup :: Nickname "matrixinvert") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

mosaic1 :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Mosaic1
mosaic1 a b c d e f g h i j k = vipsOp (Lookup :: Nickname "mosaic1") & inputs & outputs
  where
    inputs = V.ys2' a . V.xs2' b . V.yr2' c . V.xr2' d . V.ys1' e . V.xs1' f . V.yr1' g . V.xr1' h . V.direction' i . V.sec' j . V.ref' k
    outputs = V.outImg

mosaic :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Mosaic
mosaic a b c d e f g = vipsOp (Lookup :: Nickname "mosaic") & inputs & outputs
  where
    inputs = V.ysec' a . V.xsec' b . V.yref' c . V.xref' d . V.direction' e . V.sec' f . V.ref' g
    outputs = V.outMosaicResult

merge :: Int32 -> Int32 -> GV.Direction -> GV.Image -> GV.Image ->  Merge
merge a b c d e = vipsOp (Lookup :: Nickname "merge") & inputs & outputs
  where
    inputs = V.dy' a . V.dx' b . V.direction' c . V.sec' d . V.ref' e
    outputs = V.outImg

drawSmudge :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  DrawSmudge
drawSmudge a b c d e = vipsOp (Lookup :: Nickname "draw_smudge") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.top' c . V.left' d . V.image' e
    outputs = V.void

drawImage :: Int32 -> Int32 -> GV.Image -> GV.Image ->  DrawImage
drawImage a b c d = vipsOp (Lookup :: Nickname "draw_image") & inputs & outputs
  where
    inputs = V.y' a . V.x' b . V.sub' c . V.image' d
    outputs = V.void

drawFlood :: Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawFlood
drawFlood a b c d = vipsOp (Lookup :: Nickname "draw_flood") & inputs & outputs
  where
    inputs = V.y' a . V.x' b . V.ink' c . V.image' d
    outputs = V.outDrawFloodResult

drawCircle :: Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawCircle
drawCircle a b c d e = vipsOp (Lookup :: Nickname "draw_circle") & inputs & outputs
  where
    inputs = V.radius' a . V.cy' b . V.cx' c . V.ink' d . V.image' e
    outputs = V.void

drawLine :: Int32 -> Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawLine
drawLine a b c d e f = vipsOp (Lookup :: Nickname "draw_line") & inputs & outputs
  where
    inputs = V.y2' a . V.x2' b . V.y1' c . V.x1' d . V.ink' e . V.image' f
    outputs = V.void

drawMask :: Int32 -> Int32 -> GV.Image -> GV.ArrayDouble -> GV.Image ->  DrawMask
drawMask a b c d e = vipsOp (Lookup :: Nickname "draw_mask") & inputs & outputs
  where
    inputs = V.y' a . V.x' b . V.mask' c . V.ink' d . V.image' e
    outputs = V.void

drawRect :: Int32 -> Int32 -> Int32 -> Int32 -> GV.ArrayDouble -> GV.Image ->  DrawRect
drawRect a b c d e f = vipsOp (Lookup :: Nickname "draw_rect") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.top' c . V.left' d . V.ink' e . V.image' f
    outputs = V.void

fillNearest :: GV.Image ->  FillNearest
fillNearest a = vipsOp (Lookup :: Nickname "fill_nearest") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outFillNearestResult

labelregions :: GV.Image ->  Labelregions
labelregions a = vipsOp (Lookup :: Nickname "labelregions") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outLabelregionsResult

countlines :: GV.Direction -> GV.Image ->  Countlines
countlines a b = vipsOp (Lookup :: Nickname "countlines") & inputs & outputs
  where
    inputs = V.direction' a . V.img b
    outputs = V.outNolines

rank :: Int32 -> Int32 -> Int32 -> GV.Image ->  Rank
rank a b c d = vipsOp (Lookup :: Nickname "rank") & inputs & outputs
  where
    inputs = V.index' a . V.height' b . V.width' c . V.img d
    outputs = V.outImg

morph :: GV.OperationMorphology -> GV.Image -> GV.Image ->  Morph
morph a b c = vipsOp (Lookup :: Nickname "morph") & inputs & outputs
  where
    inputs = V.morph' a . V.mask' b . V.img c
    outputs = V.outImg

phasecor :: GV.Image -> GV.Image ->  Phasecor
phasecor a b = vipsOp (Lookup :: Nickname "phasecor") & inputs & outputs
  where
    inputs = V.in2' a . V.img b
    outputs = V.outImg

spectrum :: GV.Image ->  Spectrum
spectrum a = vipsOp (Lookup :: Nickname "spectrum") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

freqmult :: GV.Image -> GV.Image ->  Freqmult
freqmult a b = vipsOp (Lookup :: Nickname "freqmult") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

invfft :: GV.Image ->  Invfft
invfft a = vipsOp (Lookup :: Nickname "invfft") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

fwfft :: GV.Image ->  Fwfft
fwfft a = vipsOp (Lookup :: Nickname "fwfft") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

sobel :: GV.Image ->  Sobel
sobel a = vipsOp (Lookup :: Nickname "sobel") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

canny :: GV.Image ->  Canny
canny a = vipsOp (Lookup :: Nickname "canny") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

gaussblur :: Double -> GV.Image ->  Gaussblur
gaussblur a b = vipsOp (Lookup :: Nickname "gaussblur") & inputs & outputs
  where
    inputs = V.sigma' a . V.img b
    outputs = V.outImg

sharpen :: GV.Image ->  Sharpen
sharpen a = vipsOp (Lookup :: Nickname "sharpen") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

spcor :: GV.Image -> GV.Image ->  Spcor
spcor a b = vipsOp (Lookup :: Nickname "spcor") & inputs & outputs
  where
    inputs = V.ref' a . V.img b
    outputs = V.outImg

fastcor :: GV.Image -> GV.Image ->  Fastcor
fastcor a b = vipsOp (Lookup :: Nickname "fastcor") & inputs & outputs
  where
    inputs = V.ref' a . V.img b
    outputs = V.outImg

convasep :: GV.Image -> GV.Image ->  Convasep
convasep a b = vipsOp (Lookup :: Nickname "convasep") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

convsep :: GV.Image -> GV.Image ->  Convsep
convsep a b = vipsOp (Lookup :: Nickname "convsep") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

compass :: GV.Image -> GV.Image ->  Compass
compass a b = vipsOp (Lookup :: Nickname "compass") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

convi :: GV.Image -> GV.Image ->  Convi
convi a b = vipsOp (Lookup :: Nickname "convi") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

convf :: GV.Image -> GV.Image ->  Convf
convf a b = vipsOp (Lookup :: Nickname "convf") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

conva :: GV.Image -> GV.Image ->  Conva
conva a b = vipsOp (Lookup :: Nickname "conva") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

conv :: GV.Image -> GV.Image ->  Conv
conv a b = vipsOp (Lookup :: Nickname "conv") & inputs & outputs
  where
    inputs = V.mask' a . V.img b
    outputs = V.outImg

histEntropy :: GV.Image ->  HistEntropy
histEntropy a = vipsOp (Lookup :: Nickname "hist_entropy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

histIsmonotonic :: GV.Image ->  HistIsmonotonic
histIsmonotonic a = vipsOp (Lookup :: Nickname "hist_ismonotonic") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMonotonic

histLocal :: Int32 -> Int32 -> GV.Image ->  HistLocal
histLocal a b c = vipsOp (Lookup :: Nickname "hist_local") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.img c
    outputs = V.outImg

histPlot :: GV.Image ->  HistPlot
histPlot a = vipsOp (Lookup :: Nickname "hist_plot") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

histEqual :: GV.Image ->  HistEqual
histEqual a = vipsOp (Lookup :: Nickname "hist_equal") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

histNorm :: GV.Image ->  HistNorm
histNorm a = vipsOp (Lookup :: Nickname "hist_norm") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

histMatch :: GV.Image -> GV.Image ->  HistMatch
histMatch a b = vipsOp (Lookup :: Nickname "hist_match") & inputs & outputs
  where
    inputs = V.ref' a . V.img b
    outputs = V.outImg

histCum :: GV.Image ->  HistCum
histCum a = vipsOp (Lookup :: Nickname "hist_cum") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

stdif :: Int32 -> Int32 -> GV.Image ->  Stdif
stdif a b c = vipsOp (Lookup :: Nickname "stdif") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.img c
    outputs = V.outImg

percent :: Double -> GV.Image ->  Percent
percent a b = vipsOp (Lookup :: Nickname "percent") & inputs & outputs
  where
    inputs = V.percent' a . V.img b
    outputs = V.outThreshold

vipsCase :: GV.ArrayImage -> GV.Image ->  Case
vipsCase a b = vipsOp (Lookup :: Nickname "case") & inputs & outputs
  where
    inputs = V.cases' a . V.index' b
    outputs = V.outImg

maplut :: GV.Image -> GV.Image ->  Maplut
maplut a b = vipsOp (Lookup :: Nickname "maplut") & inputs & outputs
  where
    inputs = V.lut' a . V.img b
    outputs = V.outImg

profileLoad :: T.Text ->  ProfileLoad
profileLoad a = vipsOp (Lookup :: Nickname "profile_load") & inputs & outputs
  where
    inputs = V.name' a
    outputs = V.outProfile

xyZ2Cmyk :: GV.Image ->  XyZ2Cmyk
xyZ2Cmyk a = vipsOp (Lookup :: Nickname "XYZ2CMYK") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

cmyK2Xyz :: GV.Image ->  CmyK2Xyz
cmyK2Xyz a = vipsOp (Lookup :: Nickname "CMYK2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

scRgB2sRgb :: GV.Image ->  ScRgB2sRgb
scRgB2sRgb a = vipsOp (Lookup :: Nickname "scRGB2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

scRgB2Bw :: GV.Image ->  ScRgB2Bw
scRgB2Bw a = vipsOp (Lookup :: Nickname "scRGB2BW") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

sRgB2scRgb :: GV.Image ->  SRgB2scRgb
sRgB2scRgb a = vipsOp (Lookup :: Nickname "sRGB2scRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

dEcmc :: GV.Image -> GV.Image ->  DEcmc
dEcmc a b = vipsOp (Lookup :: Nickname "dECMC") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

dE00 :: GV.Image -> GV.Image ->  DE00
dE00 a b = vipsOp (Lookup :: Nickname "dE00") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

dE76 :: GV.Image -> GV.Image ->  DE76
dE76 a b = vipsOp (Lookup :: Nickname "dE76") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

iccTransform :: T.Text -> GV.Image ->  IccTransform
iccTransform a b = vipsOp (Lookup :: Nickname "icc_transform") & inputs & outputs
  where
    inputs = V.outputProfile' a . V.img b
    outputs = V.outImg

iccExport :: GV.Image ->  IccExport
iccExport a = vipsOp (Lookup :: Nickname "icc_export") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

iccImport :: GV.Image ->  IccImport
iccImport a = vipsOp (Lookup :: Nickname "icc_import") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

hsV2sRgb :: GV.Image ->  HsV2sRgb
hsV2sRgb a = vipsOp (Lookup :: Nickname "HSV2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

sRgB2Hsv :: GV.Image ->  SRgB2Hsv
sRgB2Hsv a = vipsOp (Lookup :: Nickname "sRGB2HSV") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

labQ2sRgb :: GV.Image ->  LabQ2sRgb
labQ2sRgb a = vipsOp (Lookup :: Nickname "LabQ2sRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

float2rad :: GV.Image ->  Float2rad
float2rad a = vipsOp (Lookup :: Nickname "float2rad") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

rad2float :: GV.Image ->  Rad2float
rad2float a = vipsOp (Lookup :: Nickname "rad2float") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lab2LabS :: GV.Image ->  Lab2LabS
lab2LabS a = vipsOp (Lookup :: Nickname "Lab2LabS") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

labS2Lab :: GV.Image ->  LabS2Lab
labS2Lab a = vipsOp (Lookup :: Nickname "LabS2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

labS2LabQ :: GV.Image ->  LabS2LabQ
labS2LabQ a = vipsOp (Lookup :: Nickname "LabS2LabQ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

labQ2LabS :: GV.Image ->  LabQ2LabS
labQ2LabS a = vipsOp (Lookup :: Nickname "LabQ2LabS") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lab2LabQ :: GV.Image ->  Lab2LabQ
lab2LabQ a = vipsOp (Lookup :: Nickname "Lab2LabQ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

labQ2Lab :: GV.Image ->  LabQ2Lab
labQ2Lab a = vipsOp (Lookup :: Nickname "LabQ2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

xyZ2scRgb :: GV.Image ->  XyZ2scRgb
xyZ2scRgb a = vipsOp (Lookup :: Nickname "XYZ2scRGB") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

scRgB2Xyz :: GV.Image ->  ScRgB2Xyz
scRgB2Xyz a = vipsOp (Lookup :: Nickname "scRGB2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

yxy2Xyz :: GV.Image ->  Yxy2Xyz
yxy2Xyz a = vipsOp (Lookup :: Nickname "Yxy2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

xyZ2Yxy :: GV.Image ->  XyZ2Yxy
xyZ2Yxy a = vipsOp (Lookup :: Nickname "XYZ2Yxy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

cmC2LCh :: GV.Image ->  CmC2LCh
cmC2LCh a = vipsOp (Lookup :: Nickname "CMC2LCh") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lCh2Cmc :: GV.Image ->  LCh2Cmc
lCh2Cmc a = vipsOp (Lookup :: Nickname "LCh2CMC") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lCh2Lab :: GV.Image ->  LCh2Lab
lCh2Lab a = vipsOp (Lookup :: Nickname "LCh2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lab2LCh :: GV.Image ->  Lab2LCh
lab2LCh a = vipsOp (Lookup :: Nickname "Lab2LCh") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

xyZ2Lab :: GV.Image ->  XyZ2Lab
xyZ2Lab a = vipsOp (Lookup :: Nickname "XYZ2Lab") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

lab2Xyz :: GV.Image ->  Lab2Xyz
lab2Xyz a = vipsOp (Lookup :: Nickname "Lab2XYZ") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

colourspace :: GV.Interpretation -> GV.Image ->  Colourspace
colourspace a b = vipsOp (Lookup :: Nickname "colourspace") & inputs & outputs
  where
    inputs = V.space' a . V.img b
    outputs = V.outImg

resize :: Double -> GV.Image ->  Resize
resize a b = vipsOp (Lookup :: Nickname "resize") & inputs & outputs
  where
    inputs = V.scale' a . V.img b
    outputs = V.outImg

rotate :: Double -> GV.Image ->  Rotate
rotate a b = vipsOp (Lookup :: Nickname "rotate") & inputs & outputs
  where
    inputs = V.angle' a . V.img b
    outputs = V.outImg

similarity :: GV.Image ->  Similarity
similarity a = vipsOp (Lookup :: Nickname "similarity") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

affine :: GV.ArrayDouble -> GV.Image ->  Affine
affine a b = vipsOp (Lookup :: Nickname "affine") & inputs & outputs
  where
    inputs = V.matrix' a . V.img b
    outputs = V.outImg

quadratic :: GV.Image -> GV.Image ->  Quadratic
quadratic a b = vipsOp (Lookup :: Nickname "quadratic") & inputs & outputs
  where
    inputs = V.coeff' a . V.img b
    outputs = V.outImg

reduce :: Double -> Double -> GV.Image ->  Reduce
reduce a b c = vipsOp (Lookup :: Nickname "reduce") & inputs & outputs
  where
    inputs = V.vshrink' a . V.hshrink' b . V.img c
    outputs = V.outImg

reducev :: Double -> GV.Image ->  Reducev
reducev a b = vipsOp (Lookup :: Nickname "reducev") & inputs & outputs
  where
    inputs = V.vshrink' a . V.img b
    outputs = V.outImg

reduceh :: Double -> GV.Image ->  Reduceh
reduceh a b = vipsOp (Lookup :: Nickname "reduceh") & inputs & outputs
  where
    inputs = V.hshrink' a . V.img b
    outputs = V.outImg

shrinkv :: Int32 -> GV.Image ->  Shrinkv
shrinkv a b = vipsOp (Lookup :: Nickname "shrinkv") & inputs & outputs
  where
    inputs = V.vshrink' a . V.img b
    outputs = V.outImg

shrinkh :: Int32 -> GV.Image ->  Shrinkh
shrinkh a b = vipsOp (Lookup :: Nickname "shrinkh") & inputs & outputs
  where
    inputs = V.hshrink' a . V.img b
    outputs = V.outImg

shrink :: Double -> Double -> GV.Image ->  Shrink
shrink a b c = vipsOp (Lookup :: Nickname "shrink") & inputs & outputs
  where
    inputs = V.vshrink' a . V.hshrink' b . V.img c
    outputs = V.outImg

mapim :: GV.Image -> GV.Image ->  Mapim
mapim a b = vipsOp (Lookup :: Nickname "mapim") & inputs & outputs
  where
    inputs = V.index' a . V.img b
    outputs = V.outImg

thumbnailSource :: Int32 -> VipsSource ->  ThumbnailSource
thumbnailSource a b = vipsOp (Lookup :: Nickname "thumbnail_source") & inputs & outputs
  where
    inputs = V.width' a . V.source' b
    outputs = V.outImg

thumbnailImage :: Int32 -> GV.Image ->  ThumbnailImage
thumbnailImage a b = vipsOp (Lookup :: Nickname "thumbnail_image") & inputs & outputs
  where
    inputs = V.width' a . V.img b
    outputs = V.outImg

thumbnailBuffer :: Int32 -> GV.Blob ->  ThumbnailBuffer
thumbnailBuffer a b = vipsOp (Lookup :: Nickname "thumbnail_buffer") & inputs & outputs
  where
    inputs = V.width' a . V.buffer' b
    outputs = V.outImg

thumbnail :: Int32 -> T.Text ->  Thumbnail
thumbnail a b = vipsOp (Lookup :: Nickname "thumbnail") & inputs & outputs
  where
    inputs = V.width' a . V.filename' b
    outputs = V.outImg

heifsaveTarget :: VipsTarget -> GV.Image ->  HeifsaveTarget
heifsaveTarget a b = vipsOp (Lookup :: Nickname "heifsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

heifsaveBuffer :: GV.Image ->  HeifsaveBuffer
heifsaveBuffer a = vipsOp (Lookup :: Nickname "heifsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

heifsave :: T.Text -> GV.Image ->  Heifsave
heifsave a b = vipsOp (Lookup :: Nickname "heifsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

fitssave :: T.Text -> GV.Image ->  Fitssave
fitssave a b = vipsOp (Lookup :: Nickname "fitssave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

magicksaveBuffer :: GV.Image ->  MagicksaveBuffer
magicksaveBuffer a = vipsOp (Lookup :: Nickname "magicksave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

magicksave :: T.Text -> GV.Image ->  Magicksave
magicksave a b = vipsOp (Lookup :: Nickname "magicksave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

tiffsaveBuffer :: GV.Image ->  TiffsaveBuffer
tiffsaveBuffer a = vipsOp (Lookup :: Nickname "tiffsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

tiffsave :: T.Text -> GV.Image ->  Tiffsave
tiffsave a b = vipsOp (Lookup :: Nickname "tiffsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

webpsaveTarget :: VipsTarget -> GV.Image ->  WebpsaveTarget
webpsaveTarget a b = vipsOp (Lookup :: Nickname "webpsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

webpsaveBuffer :: GV.Image ->  WebpsaveBuffer
webpsaveBuffer a = vipsOp (Lookup :: Nickname "webpsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

webpsave :: T.Text -> GV.Image ->  Webpsave
webpsave a b = vipsOp (Lookup :: Nickname "webpsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

jpegsaveMime :: GV.Image ->  JpegsaveMime
jpegsaveMime a = vipsOp (Lookup :: Nickname "jpegsave_mime") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.void

jpegsaveTarget :: VipsTarget -> GV.Image ->  JpegsaveTarget
jpegsaveTarget a b = vipsOp (Lookup :: Nickname "jpegsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

jpegsaveBuffer :: GV.Image ->  JpegsaveBuffer
jpegsaveBuffer a = vipsOp (Lookup :: Nickname "jpegsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

jpegsave :: T.Text -> GV.Image ->  Jpegsave
jpegsave a b = vipsOp (Lookup :: Nickname "jpegsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

pngsaveTarget :: VipsTarget -> GV.Image ->  PngsaveTarget
pngsaveTarget a b = vipsOp (Lookup :: Nickname "pngsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

pngsaveBuffer :: GV.Image ->  PngsaveBuffer
pngsaveBuffer a = vipsOp (Lookup :: Nickname "pngsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

pngsave :: T.Text -> GV.Image ->  Pngsave
pngsave a b = vipsOp (Lookup :: Nickname "pngsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

dzsaveBuffer :: GV.Image ->  DzsaveBuffer
dzsaveBuffer a = vipsOp (Lookup :: Nickname "dzsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

dzsave :: T.Text -> GV.Image ->  Dzsave
dzsave a b = vipsOp (Lookup :: Nickname "dzsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

radsaveTarget :: VipsTarget -> GV.Image ->  RadsaveTarget
radsaveTarget a b = vipsOp (Lookup :: Nickname "radsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

radsaveBuffer :: GV.Image ->  RadsaveBuffer
radsaveBuffer a = vipsOp (Lookup :: Nickname "radsave_buffer") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outBuffer

radsave :: T.Text -> GV.Image ->  Radsave
radsave a b = vipsOp (Lookup :: Nickname "radsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

ppmsaveTarget :: VipsTarget -> GV.Image ->  PpmsaveTarget
ppmsaveTarget a b = vipsOp (Lookup :: Nickname "ppmsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

ppmsave :: T.Text -> GV.Image ->  Ppmsave
ppmsave a b = vipsOp (Lookup :: Nickname "ppmsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

vipssave :: T.Text -> GV.Image ->  Vipssave
vipssave a b = vipsOp (Lookup :: Nickname "vipssave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

rawsaveFd :: Int32 -> GV.Image ->  RawsaveFd
rawsaveFd a b = vipsOp (Lookup :: Nickname "rawsave_fd") & inputs & outputs
  where
    inputs = V.fd' a . V.img b
    outputs = V.void

rawsave :: T.Text -> GV.Image ->  Rawsave
rawsave a b = vipsOp (Lookup :: Nickname "rawsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

matrixprint :: GV.Image ->  Matrixprint
matrixprint a = vipsOp (Lookup :: Nickname "matrixprint") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.void

matrixsaveTarget :: VipsTarget -> GV.Image ->  MatrixsaveTarget
matrixsaveTarget a b = vipsOp (Lookup :: Nickname "matrixsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

matrixsave :: T.Text -> GV.Image ->  Matrixsave
matrixsave a b = vipsOp (Lookup :: Nickname "matrixsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

csvsaveTarget :: VipsTarget -> GV.Image ->  CsvsaveTarget
csvsaveTarget a b = vipsOp (Lookup :: Nickname "csvsave_target") & inputs & outputs
  where
    inputs = V.target' a . V.img b
    outputs = V.void

csvsave :: T.Text -> GV.Image ->  Csvsave
csvsave a b = vipsOp (Lookup :: Nickname "csvsave") & inputs & outputs
  where
    inputs = V.filename' a . V.img b
    outputs = V.void

heifloadSource :: VipsSource ->  HeifloadSource
heifloadSource a = vipsOp (Lookup :: Nickname "heifload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outHeifloadSourceResult

heifloadBuffer :: GV.Blob ->  HeifloadBuffer
heifloadBuffer a = vipsOp (Lookup :: Nickname "heifload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outHeifloadBufferResult

heifload :: T.Text ->  Heifload
heifload a = vipsOp (Lookup :: Nickname "heifload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outHeifloadResult

openexrload :: T.Text ->  Openexrload
openexrload a = vipsOp (Lookup :: Nickname "openexrload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outOpenexrloadResult

fitsload :: T.Text ->  Fitsload
fitsload a = vipsOp (Lookup :: Nickname "fitsload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outFitsloadResult

magickloadBuffer :: GV.Blob ->  MagickloadBuffer
magickloadBuffer a = vipsOp (Lookup :: Nickname "magickload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outMagickloadBufferResult

magickload :: T.Text ->  Magickload
magickload a = vipsOp (Lookup :: Nickname "magickload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outMagickloadResult

tiffloadSource :: VipsSource ->  TiffloadSource
tiffloadSource a = vipsOp (Lookup :: Nickname "tiffload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outTiffloadSourceResult

tiffloadBuffer :: GV.Blob ->  TiffloadBuffer
tiffloadBuffer a = vipsOp (Lookup :: Nickname "tiffload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outTiffloadBufferResult

tiffload :: T.Text ->  Tiffload
tiffload a = vipsOp (Lookup :: Nickname "tiffload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outTiffloadResult

webploadSource :: VipsSource ->  WebploadSource
webploadSource a = vipsOp (Lookup :: Nickname "webpload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outWebploadSourceResult

webploadBuffer :: GV.Blob ->  WebploadBuffer
webploadBuffer a = vipsOp (Lookup :: Nickname "webpload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outWebploadBufferResult

webpload :: T.Text ->  Webpload
webpload a = vipsOp (Lookup :: Nickname "webpload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outWebploadResult

jpegloadSource :: VipsSource ->  JpegloadSource
jpegloadSource a = vipsOp (Lookup :: Nickname "jpegload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outJpegloadSourceResult

jpegloadBuffer :: GV.Blob ->  JpegloadBuffer
jpegloadBuffer a = vipsOp (Lookup :: Nickname "jpegload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outJpegloadBufferResult

jpegload :: T.Text ->  Jpegload
jpegload a = vipsOp (Lookup :: Nickname "jpegload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outJpegloadResult

pngloadSource :: VipsSource ->  PngloadSource
pngloadSource a = vipsOp (Lookup :: Nickname "pngload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outPngloadSourceResult

pngloadBuffer :: GV.Blob ->  PngloadBuffer
pngloadBuffer a = vipsOp (Lookup :: Nickname "pngload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outPngloadBufferResult

pngload :: T.Text ->  Pngload
pngload a = vipsOp (Lookup :: Nickname "pngload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outPngloadResult

gifloadSource :: VipsSource ->  GifloadSource
gifloadSource a = vipsOp (Lookup :: Nickname "gifload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outGifloadSourceResult

gifloadBuffer :: GV.Blob ->  GifloadBuffer
gifloadBuffer a = vipsOp (Lookup :: Nickname "gifload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outGifloadBufferResult

gifload :: T.Text ->  Gifload
gifload a = vipsOp (Lookup :: Nickname "gifload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outGifloadResult

svgloadSource :: VipsSource ->  SvgloadSource
svgloadSource a = vipsOp (Lookup :: Nickname "svgload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outSvgloadSourceResult

svgloadBuffer :: GV.Blob ->  SvgloadBuffer
svgloadBuffer a = vipsOp (Lookup :: Nickname "svgload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outSvgloadBufferResult

svgload :: T.Text ->  Svgload
svgload a = vipsOp (Lookup :: Nickname "svgload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outSvgloadResult

pdfloadSource :: VipsSource ->  PdfloadSource
pdfloadSource a = vipsOp (Lookup :: Nickname "pdfload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outPdfloadSourceResult

pdfloadBuffer :: GV.Blob ->  PdfloadBuffer
pdfloadBuffer a = vipsOp (Lookup :: Nickname "pdfload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outPdfloadBufferResult

pdfload :: T.Text ->  Pdfload
pdfload a = vipsOp (Lookup :: Nickname "pdfload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outPdfloadResult

radloadSource :: VipsSource ->  RadloadSource
radloadSource a = vipsOp (Lookup :: Nickname "radload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outRadloadSourceResult

radloadBuffer :: GV.Blob ->  RadloadBuffer
radloadBuffer a = vipsOp (Lookup :: Nickname "radload_buffer") & inputs & outputs
  where
    inputs = V.buffer' a
    outputs = V.outRadloadBufferResult

radload :: T.Text ->  Radload
radload a = vipsOp (Lookup :: Nickname "radload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outRadloadResult

ppmloadSource :: VipsSource ->  PpmloadSource
ppmloadSource a = vipsOp (Lookup :: Nickname "ppmload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outPpmloadSourceResult

ppmload :: T.Text ->  Ppmload
ppmload a = vipsOp (Lookup :: Nickname "ppmload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outPpmloadResult

analyzeload :: T.Text ->  Analyzeload
analyzeload a = vipsOp (Lookup :: Nickname "analyzeload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outAnalyzeloadResult

vipsload :: T.Text ->  Vipsload
vipsload a = vipsOp (Lookup :: Nickname "vipsload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outVipsloadResult

rawload :: Int32 -> Int32 -> Int32 -> T.Text ->  Rawload
rawload a b c d = vipsOp (Lookup :: Nickname "rawload") & inputs & outputs
  where
    inputs = V.bands' a . V.height' b . V.width' c . V.filename' d
    outputs = V.outRawloadResult

matrixloadSource :: VipsSource ->  MatrixloadSource
matrixloadSource a = vipsOp (Lookup :: Nickname "matrixload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outMatrixloadSourceResult

matrixload :: T.Text ->  Matrixload
matrixload a = vipsOp (Lookup :: Nickname "matrixload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outMatrixloadResult

csvloadSource :: VipsSource ->  CsvloadSource
csvloadSource a = vipsOp (Lookup :: Nickname "csvload_source") & inputs & outputs
  where
    inputs = V.source' a
    outputs = V.outCsvloadSourceResult

csvload :: T.Text ->  Csvload
csvload a = vipsOp (Lookup :: Nickname "csvload") & inputs & outputs
  where
    inputs = V.filename' a
    outputs = V.outCsvloadResult

switch :: GV.ArrayImage ->  Switch
switch a = vipsOp (Lookup :: Nickname "switch") & inputs & outputs
  where
    inputs = V.tests' a
    outputs = V.outImg

perlin :: Int32 -> Int32 ->  Perlin
perlin a b = vipsOp (Lookup :: Nickname "perlin") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

worley :: Int32 -> Int32 ->  Worley
worley a b = vipsOp (Lookup :: Nickname "worley") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

fractsurf :: Double -> Int32 -> Int32 ->  Fractsurf
fractsurf a b c = vipsOp (Lookup :: Nickname "fractsurf") & inputs & outputs
  where
    inputs = V.fractalDimension' a . V.height' b . V.width' c
    outputs = V.outImg

identity ::  Identity
identity  = vipsOp (Lookup :: Nickname "identity") & inputs & outputs
  where
    inputs = id
    outputs = V.outImg

tonelut ::  Tonelut
tonelut  = vipsOp (Lookup :: Nickname "tonelut") & inputs & outputs
  where
    inputs = id
    outputs = V.outImg

invertlut :: GV.Image ->  Invertlut
invertlut a = vipsOp (Lookup :: Nickname "invertlut") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

buildlut :: GV.Image ->  Buildlut
buildlut a = vipsOp (Lookup :: Nickname "buildlut") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

maskFractal :: Double -> Int32 -> Int32 ->  MaskFractal
maskFractal a b c = vipsOp (Lookup :: Nickname "mask_fractal") & inputs & outputs
  where
    inputs = V.fractalDimension' a . V.height' b . V.width' c
    outputs = V.outImg

maskGaussianBand :: Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskGaussianBand
maskGaussianBand a b c d e f = vipsOp (Lookup :: Nickname "mask_gaussian_band") & inputs & outputs
  where
    inputs = V.amplitudeCutoff' a . V.radius' b . V.frequencyCutoffY' c . V.frequencyCutoffX' d . V.height' e . V.width' f
    outputs = V.outImg

maskGaussianRing :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskGaussianRing
maskGaussianRing a b c d e = vipsOp (Lookup :: Nickname "mask_gaussian_ring") & inputs & outputs
  where
    inputs = V.ringwidth' a . V.amplitudeCutoff' b . V.frequencyCutoff' c . V.height' d . V.width' e
    outputs = V.outImg

maskGaussian :: Double -> Double -> Int32 -> Int32 ->  MaskGaussian
maskGaussian a b c d = vipsOp (Lookup :: Nickname "mask_gaussian") & inputs & outputs
  where
    inputs = V.amplitudeCutoff' a . V.frequencyCutoff' b . V.height' c . V.width' d
    outputs = V.outImg

maskButterworthBand :: Double -> Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworthBand
maskButterworthBand a b c d e f g = vipsOp (Lookup :: Nickname "mask_butterworth_band") & inputs & outputs
  where
    inputs = V.amplitudeCutoff' a . V.radius' b . V.frequencyCutoffY' c . V.frequencyCutoffX' d . V.order' e . V.height' f . V.width' g
    outputs = V.outImg

maskButterworthRing :: Double -> Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworthRing
maskButterworthRing a b c d e f = vipsOp (Lookup :: Nickname "mask_butterworth_ring") & inputs & outputs
  where
    inputs = V.ringwidth' a . V.amplitudeCutoff' b . V.frequencyCutoff' c . V.order' d . V.height' e . V.width' f
    outputs = V.outImg

maskButterworth :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskButterworth
maskButterworth a b c d e = vipsOp (Lookup :: Nickname "mask_butterworth") & inputs & outputs
  where
    inputs = V.amplitudeCutoff' a . V.frequencyCutoff' b . V.order' c . V.height' d . V.width' e
    outputs = V.outImg

maskIdealBand :: Double -> Double -> Double -> Int32 -> Int32 ->  MaskIdealBand
maskIdealBand a b c d e = vipsOp (Lookup :: Nickname "mask_ideal_band") & inputs & outputs
  where
    inputs = V.radius' a . V.frequencyCutoffY' b . V.frequencyCutoffX' c . V.height' d . V.width' e
    outputs = V.outImg

maskIdealRing :: Double -> Double -> Int32 -> Int32 ->  MaskIdealRing
maskIdealRing a b c d = vipsOp (Lookup :: Nickname "mask_ideal_ring") & inputs & outputs
  where
    inputs = V.ringwidth' a . V.frequencyCutoff' b . V.height' c . V.width' d
    outputs = V.outImg

maskIdeal :: Double -> Int32 -> Int32 ->  MaskIdeal
maskIdeal a b c = vipsOp (Lookup :: Nickname "mask_ideal") & inputs & outputs
  where
    inputs = V.frequencyCutoff' a . V.height' b . V.width' c
    outputs = V.outImg

sines :: Int32 -> Int32 ->  Sines
sines a b = vipsOp (Lookup :: Nickname "sines") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

zone :: Int32 -> Int32 ->  Zone
zone a b = vipsOp (Lookup :: Nickname "zone") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

grey :: Int32 -> Int32 ->  Grey
grey a b = vipsOp (Lookup :: Nickname "grey") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

eye :: Int32 -> Int32 ->  Eye
eye a b = vipsOp (Lookup :: Nickname "eye") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

logmat :: Double -> Double ->  Logmat
logmat a b = vipsOp (Lookup :: Nickname "logmat") & inputs & outputs
  where
    inputs = V.minAmpl' a . V.sigma' b
    outputs = V.outImg

gaussmat :: Double -> Double ->  Gaussmat
gaussmat a b = vipsOp (Lookup :: Nickname "gaussmat") & inputs & outputs
  where
    inputs = V.minAmpl' a . V.sigma' b
    outputs = V.outImg

xyz :: Int32 -> Int32 ->  Xyz
xyz a b = vipsOp (Lookup :: Nickname "xyz") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

text :: T.Text ->  Text
text a = vipsOp (Lookup :: Nickname "text") & inputs & outputs
  where
    inputs = V.text' a
    outputs = V.outTextResult

gaussnoise :: Int32 -> Int32 ->  Gaussnoise
gaussnoise a b = vipsOp (Lookup :: Nickname "gaussnoise") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

black :: Int32 -> Int32 ->  Black
black a b = vipsOp (Lookup :: Nickname "black") & inputs & outputs
  where
    inputs = V.height' a . V.width' b
    outputs = V.outImg

composite2 :: GV.BlendMode -> GV.Image -> GV.Image ->  Composite2
composite2 a b c = vipsOp (Lookup :: Nickname "composite2") & inputs & outputs
  where
    inputs = V.mode' a . V.overlay' b . V.base' c
    outputs = V.outImg

composite :: GV.ArrayInt -> GV.ArrayImage ->  Composite
composite a b = vipsOp (Lookup :: Nickname "composite") & inputs & outputs
  where
    inputs = V.mode' a . V.imgs b
    outputs = V.outImg

gamma :: GV.Image ->  Gamma
gamma a = vipsOp (Lookup :: Nickname "gamma") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

falsecolour :: GV.Image ->  Falsecolour
falsecolour a = vipsOp (Lookup :: Nickname "falsecolour") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

byteswap :: GV.Image ->  Byteswap
byteswap a = vipsOp (Lookup :: Nickname "byteswap") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

msb :: GV.Image ->  Msb
msb a = vipsOp (Lookup :: Nickname "msb") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

subsample :: Int32 -> Int32 -> GV.Image ->  Subsample
subsample a b c = vipsOp (Lookup :: Nickname "subsample") & inputs & outputs
  where
    inputs = V.yfac' a . V.xfac' b . V.input' c
    outputs = V.outImg

zoom :: Int32 -> Int32 -> GV.Image ->  Zoom
zoom a b c = vipsOp (Lookup :: Nickname "zoom") & inputs & outputs
  where
    inputs = V.yfac' a . V.xfac' b . V.input' c
    outputs = V.outImg

wrap :: GV.Image ->  Wrap
wrap a = vipsOp (Lookup :: Nickname "wrap") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

scale :: GV.Image ->  Scale
scale a = vipsOp (Lookup :: Nickname "scale") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

transpose3d :: GV.Image ->  Transpose3d
transpose3d a = vipsOp (Lookup :: Nickname "transpose3d") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

grid :: Int32 -> Int32 -> Int32 -> GV.Image ->  Grid
grid a b c d = vipsOp (Lookup :: Nickname "grid") & inputs & outputs
  where
    inputs = V.down' a . V.across' b . V.tileHeight' c . V.img d
    outputs = V.outImg

unpremultiply :: GV.Image ->  Unpremultiply
unpremultiply a = vipsOp (Lookup :: Nickname "unpremultiply") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

premultiply :: GV.Image ->  Premultiply
premultiply a = vipsOp (Lookup :: Nickname "premultiply") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

flatten :: GV.Image ->  Flatten
flatten a = vipsOp (Lookup :: Nickname "flatten") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

bandunfold :: GV.Image ->  Bandunfold
bandunfold a = vipsOp (Lookup :: Nickname "bandunfold") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

bandfold :: GV.Image ->  Bandfold
bandfold a = vipsOp (Lookup :: Nickname "bandfold") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

recomb :: GV.Image -> GV.Image ->  Recomb
recomb a b = vipsOp (Lookup :: Nickname "recomb") & inputs & outputs
  where
    inputs = V.m' a . V.img b
    outputs = V.outImg

ifthenelse :: GV.Image -> GV.Image -> GV.Image ->  Ifthenelse
ifthenelse a b c = vipsOp (Lookup :: Nickname "ifthenelse") & inputs & outputs
  where
    inputs = V.in2' a . V.in1' b . V.cond' c
    outputs = V.outImg

autorot :: GV.Image ->  Autorot
autorot a = vipsOp (Lookup :: Nickname "autorot") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outAutorotResult

rot45 :: GV.Image ->  Rot45
rot45 a = vipsOp (Lookup :: Nickname "rot45") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

rot :: GV.Angle -> GV.Image ->  Rot
rot a b = vipsOp (Lookup :: Nickname "rot") & inputs & outputs
  where
    inputs = V.angle' a . V.img b
    outputs = V.outImg

cast :: GV.BandFormat -> GV.Image ->  Cast
cast a b = vipsOp (Lookup :: Nickname "cast") & inputs & outputs
  where
    inputs = V.format' a . V.img b
    outputs = V.outImg

replicate :: Int32 -> Int32 -> GV.Image ->  Replicate
replicate a b c = vipsOp (Lookup :: Nickname "replicate") & inputs & outputs
  where
    inputs = V.down' a . V.across' b . V.img c
    outputs = V.outImg

bandbool :: GV.OperationBoolean -> GV.Image ->  Bandbool
bandbool a b = vipsOp (Lookup :: Nickname "bandbool") & inputs & outputs
  where
    inputs = V.boolean' a . V.img b
    outputs = V.outImg

bandmean :: GV.Image ->  Bandmean
bandmean a = vipsOp (Lookup :: Nickname "bandmean") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

bandrank :: GV.ArrayImage ->  Bandrank
bandrank a = vipsOp (Lookup :: Nickname "bandrank") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

bandjoinConst :: GV.ArrayDouble -> GV.Image ->  BandjoinConst
bandjoinConst a b = vipsOp (Lookup :: Nickname "bandjoin_const") & inputs & outputs
  where
    inputs = V.c' a . V.img b
    outputs = V.outImg

bandjoin :: GV.ArrayImage ->  Bandjoin
bandjoin a = vipsOp (Lookup :: Nickname "bandjoin") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

extractBand :: Int32 -> GV.Image ->  ExtractBand
extractBand a b = vipsOp (Lookup :: Nickname "extract_band") & inputs & outputs
  where
    inputs = V.band' a . V.img b
    outputs = V.outImg

smartcrop :: Int32 -> Int32 -> GV.Image ->  Smartcrop
smartcrop a b c = vipsOp (Lookup :: Nickname "smartcrop") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.input' c
    outputs = V.outImg

extractArea :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  ExtractArea
extractArea a b c d e = vipsOp (Lookup :: Nickname "extract_area") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.top' c . V.left' d . V.input' e
    outputs = V.outImg

arrayjoin :: GV.ArrayImage ->  Arrayjoin
arrayjoin a = vipsOp (Lookup :: Nickname "arrayjoin") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

join :: GV.Direction -> GV.Image -> GV.Image ->  Join
join a b c = vipsOp (Lookup :: Nickname "join") & inputs & outputs
  where
    inputs = V.direction' a . V.in2' b . V.in1' c
    outputs = V.outImg

insert :: Int32 -> Int32 -> GV.Image -> GV.Image ->  Insert
insert a b c d = vipsOp (Lookup :: Nickname "insert") & inputs & outputs
  where
    inputs = V.y' a . V.x' b . V.sub' c . V.main' d
    outputs = V.outImg

flip :: GV.Direction -> GV.Image ->  Flip
flip a b = vipsOp (Lookup :: Nickname "flip") & inputs & outputs
  where
    inputs = V.direction' a . V.img b
    outputs = V.outImg

gravity :: Int32 -> Int32 -> GV.CompassDirection -> GV.Image ->  Gravity
gravity a b c d = vipsOp (Lookup :: Nickname "gravity") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.direction' c . V.img d
    outputs = V.outImg

embed :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  Embed
embed a b c d e = vipsOp (Lookup :: Nickname "embed") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.y' c . V.x' d . V.img e
    outputs = V.outImg

cache :: GV.Image ->  Cache
cache a = vipsOp (Lookup :: Nickname "cache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

sequential :: GV.Image ->  Sequential
sequential a = vipsOp (Lookup :: Nickname "sequential") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

linecache :: GV.Image ->  Linecache
linecache a = vipsOp (Lookup :: Nickname "linecache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

tilecache :: GV.Image ->  Tilecache
tilecache a = vipsOp (Lookup :: Nickname "tilecache") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

copy :: GV.Image ->  Copy
copy a = vipsOp (Lookup :: Nickname "copy") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

findTrim :: GV.Image ->  FindTrim
findTrim a = vipsOp (Lookup :: Nickname "find_trim") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outFindTrimResult

getpoint :: Int32 -> Int32 -> GV.Image ->  Getpoint
getpoint a b c = vipsOp (Lookup :: Nickname "getpoint") & inputs & outputs
  where
    inputs = V.y' a . V.x' b . V.img c
    outputs = V.outOutArray

measure :: Int32 -> Int32 -> GV.Image ->  Measure
measure a b c = vipsOp (Lookup :: Nickname "measure") & inputs & outputs
  where
    inputs = V.v' a . V.h' b . V.img c
    outputs = V.outImg

profile :: GV.Image ->  Profile
profile a = vipsOp (Lookup :: Nickname "profile") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outProfileResult

project :: GV.Image ->  Project
project a = vipsOp (Lookup :: Nickname "project") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outProjectResult

houghCircle :: GV.Image ->  HoughCircle
houghCircle a = vipsOp (Lookup :: Nickname "hough_circle") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

houghLine :: GV.Image ->  HoughLine
houghLine a = vipsOp (Lookup :: Nickname "hough_line") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

histFindIndexed :: GV.Image -> GV.Image ->  HistFindIndexed
histFindIndexed a b = vipsOp (Lookup :: Nickname "hist_find_indexed") & inputs & outputs
  where
    inputs = V.index' a . V.img b
    outputs = V.outImg

histFindNdim :: GV.Image ->  HistFindNdim
histFindNdim a = vipsOp (Lookup :: Nickname "hist_find_ndim") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

histFind :: GV.Image ->  HistFind
histFind a = vipsOp (Lookup :: Nickname "hist_find") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

stats :: GV.Image ->  Stats
stats a = vipsOp (Lookup :: Nickname "stats") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

deviate :: GV.Image ->  Deviate
deviate a = vipsOp (Lookup :: Nickname "deviate") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

max :: GV.Image ->  Max
max a = vipsOp (Lookup :: Nickname "max") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMaxResult

min :: GV.Image ->  Min
min a = vipsOp (Lookup :: Nickname "min") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outMinResult

avg :: GV.Image ->  Avg
avg a = vipsOp (Lookup :: Nickname "avg") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outOut

complexget :: GV.OperationComplexget -> GV.Image ->  Complexget
complexget a b = vipsOp (Lookup :: Nickname "complexget") & inputs & outputs
  where
    inputs = V.get' a . V.img b
    outputs = V.outImg

complex :: GV.OperationComplex -> GV.Image ->  Complex
complex a b = vipsOp (Lookup :: Nickname "complex") & inputs & outputs
  where
    inputs = V.cmplx' a . V.img b
    outputs = V.outImg

math2Const :: GV.ArrayDouble -> GV.OperationMath2 -> GV.Image ->  Math2Const
math2Const a b c = vipsOp (Lookup :: Nickname "math2_const") & inputs & outputs
  where
    inputs = V.c' a . V.math2' b . V.img c
    outputs = V.outImg

booleanConst :: GV.ArrayDouble -> GV.OperationBoolean -> GV.Image ->  BooleanConst
booleanConst a b c = vipsOp (Lookup :: Nickname "boolean_const") & inputs & outputs
  where
    inputs = V.c' a . V.boolean' b . V.img c
    outputs = V.outImg

remainderConst :: GV.ArrayDouble -> GV.Image ->  RemainderConst
remainderConst a b = vipsOp (Lookup :: Nickname "remainder_const") & inputs & outputs
  where
    inputs = V.c' a . V.img b
    outputs = V.outImg

relationalConst :: GV.ArrayDouble -> GV.OperationRelational -> GV.Image ->  RelationalConst
relationalConst a b c = vipsOp (Lookup :: Nickname "relational_const") & inputs & outputs
  where
    inputs = V.c' a . V.relational' b . V.img c
    outputs = V.outImg

round :: GV.OperationRound -> GV.Image ->  Round
round a b = vipsOp (Lookup :: Nickname "round") & inputs & outputs
  where
    inputs = V.round' a . V.img b
    outputs = V.outImg

sign :: GV.Image ->  Sign
sign a = vipsOp (Lookup :: Nickname "sign") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

abs :: GV.Image ->  Abs
abs a = vipsOp (Lookup :: Nickname "abs") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

math :: GV.OperationMath -> GV.Image ->  Math
math a b = vipsOp (Lookup :: Nickname "math") & inputs & outputs
  where
    inputs = V.math' a . V.img b
    outputs = V.outImg

linear :: GV.ArrayDouble -> GV.ArrayDouble -> GV.Image ->  Linear
linear a b c = vipsOp (Lookup :: Nickname "linear") & inputs & outputs
  where
    inputs = V.b' a . V.a' b . V.img c
    outputs = V.outImg

invert :: GV.Image ->  Invert
invert a = vipsOp (Lookup :: Nickname "invert") & inputs & outputs
  where
    inputs = V.img a
    outputs = V.outImg

sum :: GV.ArrayImage ->  Sum
sum a = vipsOp (Lookup :: Nickname "sum") & inputs & outputs
  where
    inputs = V.imgs a
    outputs = V.outImg

complexform :: GV.Image -> GV.Image ->  Complexform
complexform a b = vipsOp (Lookup :: Nickname "complexform") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

complex2 :: GV.OperationComplex2 -> GV.Image -> GV.Image ->  Complex2
complex2 a b c = vipsOp (Lookup :: Nickname "complex2") & inputs & outputs
  where
    inputs = V.cmplx' a . V.right' b . V.left' c
    outputs = V.outImg

math2 :: GV.OperationMath2 -> GV.Image -> GV.Image ->  Math2
math2 a b c = vipsOp (Lookup :: Nickname "math2") & inputs & outputs
  where
    inputs = V.math2' a . V.right' b . V.left' c
    outputs = V.outImg

boolean :: GV.OperationBoolean -> GV.Image -> GV.Image ->  Boolean
boolean a b c = vipsOp (Lookup :: Nickname "boolean") & inputs & outputs
  where
    inputs = V.boolean' a . V.right' b . V.left' c
    outputs = V.outImg

remainder :: GV.Image -> GV.Image ->  Remainder
remainder a b = vipsOp (Lookup :: Nickname "remainder") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

relational :: GV.OperationRelational -> GV.Image -> GV.Image ->  Relational
relational a b c = vipsOp (Lookup :: Nickname "relational") & inputs & outputs
  where
    inputs = V.relational' a . V.right' b . V.left' c
    outputs = V.outImg

divide :: GV.Image -> GV.Image ->  Divide
divide a b = vipsOp (Lookup :: Nickname "divide") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

multiply :: GV.Image -> GV.Image ->  Multiply
multiply a b = vipsOp (Lookup :: Nickname "multiply") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

subtract :: GV.Image -> GV.Image ->  Subtract
subtract a b = vipsOp (Lookup :: Nickname "subtract") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

add :: GV.Image -> GV.Image ->  Add
add a b = vipsOp (Lookup :: Nickname "add") & inputs & outputs
  where
    inputs = V.right' a . V.left' b
    outputs = V.outImg

system :: T.Text ->  System
system a = vipsOp (Lookup :: Nickname "system") & inputs & outputs
  where
    inputs = V.cmdFormat' a
    outputs = V.outSystemResult

crop :: Int32 -> Int32 -> Int32 -> Int32 -> GV.Image ->  Crop
crop a b c d e = vipsOp (Lookup :: Nickname "crop") & inputs & outputs
  where
    inputs = V.height' a . V.width' b . V.top' c . V.left' d . V.input' e
    outputs = V.outImg
