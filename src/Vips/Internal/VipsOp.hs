{-# LANGUAGE DataKinds, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeApplications #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Internal.VipsOp
        ( IsVipsArg, toGValue, clearArg
        , IsVipsOutput, gValueType, fromGValue
        , setProperty, getProperty, getNone
        , vipsOp, vipsForeignOp, runOp
        )
where

import qualified  Data.Text as T

import            Data.GI.Base.ShortPrelude (liftIO, GType)
import            Data.GI.Base.GType (gtypeInvalid)
import            Data.GI.Base.GValue (GValue)
import qualified  Data.GI.Base.GValue as GValue (toGValue, fromGValue, newGValue, unsetGValue, gvalueGType_)
import qualified  Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified  GI.GObject.Objects.Object as GObject
import qualified  GI.Vips as GV

import            Vips.VipsIO

--
-- Vips operations FFI via GObject introspection
--

-- |Values that may be used as an input to a vips operation
class IsVipsArg a where
  toGValue :: a -> IO GValue
  clearArg :: a -> IO ()

instance IsVipsArg Double where
  toGValue = GValue.toGValue
  clearArg _ = return ()

instance IsVipsArg T.Text where
  toGValue = GValue.toGValue . Just
  clearArg _ = return ()                -- TODO: confirm if this needs GObject.objectUnref or some such

instance IsVipsArg GV.Image where
  toGValue = GValue.toGValue . Just
  clearArg = GObject.objectUnref


-- |Values that may be retrieved as outputs from a vips operation
class IsVipsOutput a where
  gValueType :: IO GType
  fromGValue :: GValue -> IO (Maybe a)

-- |This is never used, but is necessary to allow IsVipsOp instances
instance IsVipsOutput () where
  gValueType = return gtypeInvalid
  fromGValue _ = return $ Just ()

instance IsVipsOutput GV.Image where
  gValueType = GValue.gvalueGType_ @(Maybe GV.Image)
  fromGValue = GValue.fromGValue


type Op = GV.Operation

-- |Construct a named vips operation
vipsOp :: T.Text -> VipsIO Op
vipsOp = mkOp'

-- |Construct a vips foreign operation;
--  that is, an operation whose name is derived by some libvips foreign helper function
vipsForeignOp :: VipsIO T.Text -> VipsIO Op
vipsForeignOp f = mkOp' =<< f

mkOp' :: T.Text -> VipsIO Op
mkOp' = GV.operationNew

-- |Execute a vips operation
runOp :: Op -> VipsIO Op
runOp op = liftIO $ do
  result <- GV.cacheOperationBuild op
  clearOp' op
  return result

-- |Clear the GObject references for a vips operation
clearOp' :: Op -> IO ()
clearOp' op = do
  GV.objectUnrefOutputs op
  GObject.objectUnref op

setProperty :: (IsVipsArg a) => T.Text -> a -> Op -> VipsIO Op
setProperty l a op = liftIO $ do
  GObject.objectSetProperty op l =<< toGValue a
  clearArg a
  return op

getProperty :: forall a. (IsVipsOutput a) => T.Text -> Op -> VipsIO (Maybe a)
getProperty l op = liftIO $ do
  v <- GValue.newGValue =<< gValueType @a
  GObject.objectGetProperty op l v
  result <- fromGValue v
  B.ManagedPtr.withManagedPtr v GValue.unsetGValue
  clearOp' op
  return result

-- |Alternative to `getProperty` for vips operations that
-- |do not return a result.
--  N.B. some result function _must_ be called to clear the
--  GObject references.
getNone :: Op -> VipsIO ()
getNone op = liftIO $ clearOp' op
