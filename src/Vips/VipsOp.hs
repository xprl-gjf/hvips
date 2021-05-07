{-# LANGUAGE DataKinds, KindSignatures #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.VipsOp
        ( Nickname(..)
        , VipsOp(..)
        , V.IsVipsArg, setInput
        , V.getProperty, void
        , vipsOp
        , V.vipsForeignOp
        , runVips
        )
where

import qualified  Data.Text as T

import            Vips.VipsIO
import qualified  Vips.Internal.VipsOp as V
import qualified  GI.Vips as GV
import            GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data Nickname (l :: Symbol) = Lookup

data VipsOp (l :: Symbol) a = VipsOp
        { getOp :: VipsIO GV.Operation
        , getOutput :: GV.Operation -> VipsIO (Maybe a)
        }

vipsOp :: (KnownSymbol l, V.IsVipsOutput a) => Nickname l -> T.Text -> VipsOp l a
vipsOp l out = VipsOp { getOp = V.vipsOp (opName l), getOutput = V.getProperty out }
  where
    opName = T.pack . symbolVal

runVips :: VipsOp l a -> VipsIO (Maybe a)
runVips op = getOp op >>= V.runOp >>= getOutput op


setInput :: (V.IsVipsArg a) => T.Text -> a -> VipsOp l b -> VipsOp l b
setInput t a v = v { getOp = getOp' }
  where
    getOp' = getOp v >>= V.setProperty t a


void :: GV.Operation -> VipsIO (Maybe ())
void = fmap Just . V.getNone
