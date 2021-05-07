{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses,
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
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified GI.Vips as GV
import           Vips.VipsOp

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
type LoadImage = VipsOp "foreignLoadImage" GV.Image
loadImage :: FilePath -> LoadImage
loadImage a = vipsForeignOp loader (Foreign :: Nickname "foreignLoadImage") & inputs & outputs
  where
    a' = T.pack a
    loader = GV.foreignFindLoad a'
    inputs = filename a'
    outputs = outImg
instance HasArgument LoadImage "filename" T.Text
instance HasResult LoadImage "out" GV.Image

type SaveImage = VipsOp "foreignSaveImage" ()
saveImage :: FilePath -> GV.Image -> SaveImage
saveImage a b = vipsForeignOp saver (Foreign :: Nickname "foreignSaveImage") & inputs & outputs
  where
    a' = T.pack a
    saver = GV.foreignFindSave a'
    inputs = filename a' . img b
    outputs = void
instance HasArgument SaveImage "in" GV.Image
instance HasArgument SaveImage "filename" T.Text

--
-- Vips image operations:
--
type Invert = VipsOp "invert" GV.Image
invert :: GV.Image -> Invert
invert a = vipsOp (Lookup :: Nickname "invert") & inputs & outputs
  where
    inputs = img a
    outputs = outImg
instance HasArgument Invert "in" GV.Image
instance HasResult Invert "out" GV.Image

type GaussBlur = VipsOp "gaussBlur" GV.Image
gaussBlur :: Double -> GV.Image -> GaussBlur
gaussBlur a b = vipsOp (Lookup :: Nickname "gaussBlur") & inputs & outputs
  where
    inputs = sigma a . img b
    outputs = outImg
instance HasArgument GaussBlur "in" GV.Image
instance HasArgument GaussBlur "sigma" Double
instance HasArgument GaussBlur "min_ampl" Double
instance HasResult GaussBlur "out" GV.Image

--
-- Named argument handling:
--
data Argument (l :: Symbol) = Set | Get

class IsVipsOp a where
  setInput' :: (IsVipsArg b) => T.Text -> b -> a -> a
  setOutput' :: T.Text -> a -> a

instance (IsVipsOutput a) => IsVipsOp (VipsOp m a) where
  setInput' = setInput
  setOutput' = setOutput

class (KnownSymbol l, IsVipsOp a, IsVipsArg b)  => HasArgument a l b where
  -- |Apply a named argument to a Vips operation
  set :: Argument l -> b -> a -> a
  set l = setInput' (attrName l)

class (KnownSymbol l, IsVipsOp a, IsVipsOutput b) => HasResult a l b where
  -- |Get a named result from a Vips operation
  get :: Argument l -> proxy b -> a -> a
  get l _ = setOutput' (attrName l)

attrName :: (KnownSymbol l) => Argument l -> T.Text
attrName = T.pack . symbolVal

--
-- Vips named result properties
--
outImg :: (HasResult a "out" GV.Image) => a -> a
outImg = get (Get :: Argument "out") (Proxy :: Proxy GV.Image)

--
-- Vips named arguments:
--

-- |"img" is a synonym for "in",
--  since `in` is a Haskell reserved word
img :: (HasArgument a "in" GV.Image) => GV.Image -> a -> a
img = set (Set :: Argument "in")

filename :: (HasArgument a "filename" b) => b -> a -> a
filename = set (Set :: Argument "filename")

sigma :: (HasArgument a "sigma" b) => b -> a -> a
sigma = set (Set :: Argument "sigma")

minAmpl :: (HasArgument a "min_ampl" b) => b -> a -> a
minAmpl = set (Set :: Argument "min_ampl")
