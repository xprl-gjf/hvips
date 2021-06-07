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
