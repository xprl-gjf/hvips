{-# LANGUAGE DataKinds, KindSignatures,
             MultiParamTypeClasses #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Internal.VipsOp
        ( Nickname(..)
        , VipsResult, getProperty
        , VipsOp, setInput, setOutput, void
        , V.IsVipsArg, V.IsVipsOutput
        , vipsOp, vipsForeignOp
        , runVips
        )
where

import qualified  Data.Text as T
import            GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import            Vips.VipsIO (VipsIO)
import qualified  Vips.Internal.VipsOp.GI as V
import qualified  GI.Vips as GV (Operation)

-- |Symbolic name for a Vips operation
data Nickname (l :: Symbol) = Lookup | Foreign

-- |Type-safe wrapper for a built (and executed) Operation
newtype VipsResult = VipsResult { fromResult :: GV.Operation }


-- |Get a result output property
getProperty :: (V.IsVipsOutput a) => T.Text -> VipsResult -> VipsIO a
getProperty t = V.getProperty t . fromResult


-- |A named Vips operation
data VipsOp (l :: Symbol) a = VipsOp
        { getOp :: VipsIO GV.Operation          -- ^construct the Vips operation
        , getOutput :: VipsResult -> VipsIO a   -- ^get the result from the Vips operation
        }

-- |Constructor for a named vips operation
vipsOp :: (KnownSymbol l) => Nickname l -> VipsOp l a
vipsOp l = VipsOp { getOp = V.vipsOp (opName l), getOutput = undefined }
  where
    opName = T.pack . symbolVal

-- |Constructor for a vips foreign operation
vipsForeignOp :: VipsIO T.Text -> Nickname l -> VipsOp l a
vipsForeignOp n _ = VipsOp { getOp = V.vipsForeignOp n, getOutput = undefined }

-- |Execute a VipsOp and return the result
runVips :: VipsOp l a -> VipsIO a
runVips op = do
  result <- VipsResult <$> (getOp op >>= V.runOp)
  getOutput op result

-- |Set an input argument value on a VipsOp
setInput :: (V.IsVipsArg a)
  => T.Text         -- ^the argument name
  -> a              -- ^the argument value
  -> VipsOp l b     -- ^the operation to which the argument will be applied
  -> VipsOp l b     -- ^returns the modified operation
setInput t a v = v { getOp = getOp' }
  where
    getOp' = getOp v >>= V.setProperty t a

-- |Set the result output function for a VipsOp
setOutput :: (VipsResult -> VipsIO a)   -- ^the result output function
          -> VipsOp l a                 -- ^the operation from which the result will be retrieved
          -> VipsOp l a
setOutput x op = op { getOutput = x }

-- |Set the result function for a VipsOp that has no return value
void :: VipsOp l () -> VipsOp l ()
void op = op { getOutput = returnVoid' }

-- |Convenient return value function for a VipsOp
-- |that has no result.
--  N.B. some return value function _must_ be called to ensure that the
--  underlying GObject is unreferenced and disposed.
returnVoid' :: VipsResult -> VipsIO ()
returnVoid' = V.getNone . fromResult
