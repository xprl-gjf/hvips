{-# LANGUAGE OverloadedStrings, OverloadedLabels,
             DataKinds, TypeFamilies, MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, AllowAmbiguousTypes #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Operations
  ( loadImage
  , saveImage
  , invert
  , gaussBlur
  , minAmpl
  , (<&>)
  ) where

import           Data.Function ((&))
import qualified Data.Text as T

import           GHC.OverloadedLabels (IsLabel(..))
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified GI.Vips as GV
import           Vips.VipsOp

--
-- Vips image operations:
--
type LoadImage = VipsOp "foreignLoadImage" GV.Image

loadImage :: FilePath -> LoadImage
loadImage a = VipsOp { getOp = vipsForeignOp loader , getOutput = getProperty "out"} & setInputs
  where
    filename = T.pack a
    loader = GV.foreignFindLoad filename
    setInputs = setInput "filename" filename


type SaveImage = VipsOp "foreignSaveImage" ()

saveImage :: FilePath -> GV.Image -> SaveImage
saveImage a img = VipsOp { getOp = vipsForeignOp saver, getOutput = void } & setInputs
  where
    filename = T.pack a
    saver = GV.foreignFindSave filename
    setInputs = setInput "filename" filename . setInput "in" img


type Invert = VipsOp "invert" GV.Image

invert :: GV.Image -> Invert
invert img = vipsOp (Lookup :: Nickname "invert") "out" & setInputs
  where
    setInputs = setInput "in" img


type GaussBlur = VipsOp "gaussBlur" GV.Image

gaussBlur :: Double -> GV.Image -> GaussBlur
gaussBlur sigma img = vipsOp (Lookup :: Nickname "gaussBlur") "out" & setInputs
  where
    setInputs = setInput "sigma" sigma . setInput "in" img

instance HasAttribute GaussBlur "min_ampl" Double where set = apply


--
-- Attribute setters
--

data AttrName (l :: Symbol) = Set

class HasAttribute a l b where
  set :: AttrName l -> b -> a -> a

apply :: (KnownSymbol l, IsVipsArg a) => AttrName l -> a -> VipsOp m b -> VipsOp m b
apply l = setInput (attrName l)
  where
    attrName = T.pack . symbolVal

instance (HasAttribute a l b) => IsLabel l (b -> a -> a) where
  fromLabel = set (Set :: AttrName l)


-- |Convenient reverse function application for applying attributes
-- |to partially constructed Vips operations.
--
-- Example usage:
--   gaussBlur 1.2 <&> min_ampl (1.8 :: Double)
(<&>) :: (a -> b) -> (b -> b) -> (a -> b)
infixl 4 <&>
f <&> g = g . f


minAmpl :: (HasAttribute a "min_ampl" b) => b -> a -> a
minAmpl = #min_ampl
