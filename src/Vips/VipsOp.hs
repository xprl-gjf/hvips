{-# LANGUAGE DataKinds, KindSignatures #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.VipsOp
        ( Nickname(..)
        , VipsOp(..)
        , V.IsVipsArg, setInput
        , V.IsVipsOutput, setOutput, void
        , V.getProperty
        , vipsOp, vipsForeignOp
        , runVips
        )
where

import qualified  Data.Text as T
import            GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import            Vips.VipsIO
import qualified  Vips.Internal.VipsOp as V
import qualified  GI.Vips as GV

-- |Symbolic name for a Vips operation
data Nickname (l :: Symbol) = Lookup | Foreign

-- |A named Vips operation
data VipsOp (l :: Symbol) a = VipsOp
        { getOp :: VipsIO GV.Operation                   -- ^construct the Vips operation
        , getOutput :: GV.Operation -> VipsIO (Maybe a)  -- ^get the result from the Vips operation
        }


-- |Constructor for a named vips operation
vipsOp :: (KnownSymbol l) => Nickname l -> VipsOp l a
vipsOp l = VipsOp { getOp = V.vipsOp (opName l), getOutput = const $ return Nothing }
  where
    opName = T.pack . symbolVal

-- |Constructor for a vips foreign operation
vipsForeignOp :: VipsIO T.Text -> Nickname l -> VipsOp l a
vipsForeignOp f _ = VipsOp { getOp = V.vipsForeignOp f, getOutput = const $ return Nothing }

-- |Execute a VipsOp and return the result
runVips :: VipsOp l a -> VipsIO (Maybe a)
runVips op = getOp op >>= V.runOp >>= getOutput op

-- |Set an input argument value on a VipsOp
setInput :: (V.IsVipsArg a) =>
  T.Text ->       -- ^the argument name
  a ->            -- ^the argument value
  VipsOp l b ->   -- ^the operation to which the argument will be applied
  VipsOp l b      -- ^returns the modified operation
setInput t a v = v { getOp = getOp' }
  where
    getOp' = getOp v >>= V.setProperty t a

-- |Set the result argument name for a VipsOp
setOutput :: (V.IsVipsOutput a) =>
  T.Text ->       -- ^the result argument name
  VipsOp l a ->   -- ^the operation from which the result will be retrieved
  VipsOp l a
setOutput t op = op { getOutput = V.getProperty t }

-- |Set the result function for a VipsOp that has no return value
void :: VipsOp l () -> VipsOp l ()
void op = op { getOutput = returnVoid }

-- |Convenient return value function for a VipsOp
-- |that has no result.
--  N.B. some return value function _must_ be called to ensure that the
--  underlying GObject is unreferenced and disposed.
returnVoid :: GV.Operation -> VipsIO (Maybe ())
returnVoid = fmap Just . V.getNone
