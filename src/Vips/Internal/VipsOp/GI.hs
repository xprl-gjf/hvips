{-# LANGUAGE DataKinds, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeApplications #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Internal.VipsOp.GI
        ( IsVipsArg, toGValue, clearArg
        , IsVipsOutput, gValueType, fromGValue
        , setProperty, getProperty, getNone
        , vipsOp, vipsForeignOp, runOp
        )
where

import            Data.Convertible.Base
import            Data.Convertible.Instances.C ()
import            Data.Int
import            Data.Word
import qualified  Data.Text as T

import            Data.GI.Base.ShortPrelude (liftIO, GType)
import            Data.GI.Base.GType (gtypeInvalid)
import            Data.GI.Base.GValue (GValue(..))
import qualified  Data.GI.Base.GValue as GValue
                    ( toGValue, fromGValue, newGValue, buildGValue, unsetGValue
                    , set_enum
                    , gvalueGType_
                    )
import qualified  Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified  GI.GObject.Objects.Object as GObject
import qualified  GI.Vips as GV

import            Vips.VipsIO

--
-- Vips operations FFI via GObject Introspection
--

--
-- |Values that may be used as an input to a vips operation
--
class IsVipsArg a where
  toGValue :: a -> IO GValue
  clearArg :: a -> IO ()
  clearArg _ = return ()

instance IsVipsArg Int32 where
  toGValue = GValue.toGValue

instance IsVipsArg Word64 where
  toGValue = GValue.toGValue

instance IsVipsArg Double where
  toGValue = GValue.toGValue

instance IsVipsArg Bool where
  toGValue = GValue.toGValue

instance IsVipsArg T.Text where
  toGValue = GValue.toGValue . Just
  -- TODO: confirm if this needs clearArg to call GObject.objectUnref or some such

instance IsVipsArg GV.Blob where
  toGValue = GValue.toGValue . Just
  -- TODO: confirm if this needs clearArg to call GObject.objectUnref or some such

instance IsVipsArg GV.Image where
  toGValue = GValue.toGValue . Just
  clearArg = GObject.objectUnref

instance IsVipsArg GV.Interpolate where
  toGValue = GValue.toGValue . Just
  clearArg = GObject.objectUnref

instance IsVipsArg GV.ArrayImage where
  toGValue = GValue.toGValue . Just

instance IsVipsArg GV.ArrayInt where
  toGValue = GValue.toGValue . Just

instance IsVipsArg GV.ArrayDouble where
  toGValue = GValue.toGValue . Just

instance IsVipsArg GV.Access where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Access

instance IsVipsArg GV.Align where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Align

instance IsVipsArg GV.Angle where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Angle

instance IsVipsArg GV.Angle45 where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Angle45

instance IsVipsArg GV.BandFormat where
  toGValue x = enumToGValue x =<< GV.glibType @GV.BandFormat

instance IsVipsArg GV.BlendMode where
  toGValue x = enumToGValue x =<< GV.glibType @GV.BlendMode

instance IsVipsArg GV.Coding where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Coding

instance IsVipsArg GV.Combine where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Combine

instance IsVipsArg GV.CombineMode where
  toGValue x = enumToGValue x =<< GV.glibType @GV.CombineMode

instance IsVipsArg GV.CompassDirection where
  toGValue x = enumToGValue x =<< GV.glibType @GV.CompassDirection

instance IsVipsArg GV.Direction where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Direction

instance IsVipsArg GV.Extend where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Extend

instance IsVipsArg GV.Intent where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Intent

instance IsVipsArg GV.Interesting where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Interesting

instance IsVipsArg GV.Interpretation where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Interpretation

instance IsVipsArg GV.Kernel where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Kernel

instance IsVipsArg GV.OperationBoolean where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationBoolean

instance IsVipsArg GV.OperationRelational where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationRelational

instance IsVipsArg GV.OperationMath where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationMath

instance IsVipsArg GV.OperationMath2 where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationMath2

instance IsVipsArg GV.OperationRound where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationRound

instance IsVipsArg GV.OperationComplex where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationComplex

instance IsVipsArg GV.OperationComplex2 where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationComplex2

instance IsVipsArg GV.OperationComplexget where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationComplexget

instance IsVipsArg GV.OperationMorphology where
  toGValue x = enumToGValue x =<< GV.glibType @GV.OperationMorphology

instance IsVipsArg GV.PCS where
  toGValue x = enumToGValue x =<< GV.glibType @GV.PCS

instance IsVipsArg GV.Precision where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Precision

instance IsVipsArg GV.RegionShrink where
  toGValue x = enumToGValue x =<< GV.glibType @GV.RegionShrink

instance IsVipsArg GV.Size where
  toGValue x = enumToGValue x =<< GV.glibType @GV.Size

instance IsVipsArg GV.ForeignDzContainer where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignDzContainer

instance IsVipsArg GV.ForeignDzDepth where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignDzDepth

instance IsVipsArg GV.ForeignDzLayout where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignDzLayout

instance IsVipsArg GV.ForeignJpegSubsample where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignJpegSubsample

instance IsVipsArg GV.ForeignPngFilter where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignPngFilter

instance IsVipsArg GV.ForeignHeifCompression where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignHeifCompression

instance IsVipsArg GV.ForeignTiffCompression where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignTiffCompression

instance IsVipsArg GV.ForeignTiffPredictor where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignTiffPredictor

instance IsVipsArg GV.ForeignTiffResunit where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignTiffResunit

instance IsVipsArg GV.ForeignWebpPreset where
  toGValue x = enumToGValue x =<< GV.glibType @GV.ForeignWebpPreset

enumToGValue :: (Enum a) => a -> GV.GType -> IO GValue
enumToGValue x t = GValue.buildGValue t set x
  where
    set gv = GValue.set_enum gv . convert . fromEnum

--
-- |Values that may be retrieved as outputs from a vips operation
--
class IsVipsOutput a where
  gValueType :: IO GType
  fromGValue :: GValue -> IO (Maybe a)

instance IsVipsOutput () where        -- ^this is never used, but is necessary to allow IsVipsOp instances
  gValueType = return gtypeInvalid
  fromGValue _ = return $ Just ()

instance IsVipsOutput Bool where
  gValueType = GValue.gvalueGType_ @Bool
  fromGValue = fmap Just . GValue.fromGValue

instance IsVipsOutput Int32 where
  gValueType = GValue.gvalueGType_ @Int32
  fromGValue = fmap Just . GValue.fromGValue

instance IsVipsOutput Double where
  gValueType = GValue.gvalueGType_ @Double
  fromGValue = fmap Just . GValue.fromGValue

instance IsVipsOutput T.Text where
  gValueType = GValue.gvalueGType_ @(Maybe T.Text)
  fromGValue = GValue.fromGValue

instance IsVipsOutput GV.Image where
  gValueType = GValue.gvalueGType_ @(Maybe GV.Image)
  fromGValue = GValue.fromGValue

instance IsVipsOutput GV.ArrayInt where
  gValueType = GValue.gvalueGType_ @(Maybe GV.ArrayInt)
  fromGValue = GValue.fromGValue

instance IsVipsOutput GV.ArrayDouble where
  gValueType = GValue.gvalueGType_ @(Maybe GV.ArrayDouble)
  fromGValue = GValue.fromGValue

instance IsVipsOutput GV.Blob where
  gValueType = GValue.gvalueGType_ @(Maybe GV.Blob)
  fromGValue = GValue.fromGValue

instance IsVipsOutput GV.Angle where
  gValueType = GV.glibType @GV.Angle
  -- FIXME
  fromGValue = undefined

instance IsVipsOutput GV.ForeignFlags where
  gValueType = GV.glibType @GV.ForeignFlags
  -- FIXME
  fromGValue = undefined



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
