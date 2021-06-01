{-# LANGUAGE OverloadedStrings, TypeApplications #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Introspect
        ( typeMap
        , argumentMap
        , listVipsOperations
        , listVipsOperationArgs
        , VipsOperationArgInfo(..)
        )
where

import           Data.Int
import           Data.IORef
import qualified Data.Text as T
import qualified Foreign.Ptr as FP
import qualified Foreign.StablePtr as FSP
import qualified Foreign.ForeignPtr as FFP
import qualified GHC.Stack as S

import qualified Data.GI.Base.BasicTypes as B (GParamSpec, GType, CGType, gtypeToCGType, glibType)
import qualified Data.GI.Base.GValue as GValue (gvalueType)
import qualified GI.GObject.Objects.ParamSpec as ParamSpec
import qualified GI.Vips as V

--
-- vips operations type mapping via FFI
--

-- |Get the list of GTypes for all known Vips operations
--
listVipsOperations :: (S.HasCallStack) => IO [B.GType]
listVipsOperations = do
  vipsOperations' <- newIORef ([] :: [B.GType])
  vipsOperationsPtr <- FSP.newStablePtr vipsOperations'
  t <- B.glibType @V.Operation
  _ <- typeMap t listVipsOperation' (FSP.castStablePtrToPtr vipsOperationsPtr) FP.nullPtr
  FSP.freeStablePtr vipsOperationsPtr
  readIORef vipsOperations'

-- |Add a vips operation GType to the list of all known vips operations
listVipsOperation' :: (S.HasCallStack)
  => B.GType                    -- ^ the GType for the Vips operation that has been found
  -> FP.Ptr ()                  -- ^ client data ptr; expected to be a StablePtr to an IORef [GType]
  -> FP.Ptr ()                  -- ^ client data ptr; ignored
  -> IO (FP.Ptr ())
listVipsOperation' t a _ = do
  let pVipsOps = FSP.castPtrToStablePtr a :: FSP.StablePtr (IORef [B.GType])
  vipsOps <- FSP.deRefStablePtr pVipsOps
  modifyIORef' vipsOps (t:)
  typeMap t listVipsOperation' a FP.nullPtr   -- recursively fetch subtypes

foreign import ccall "vips_type_map" vips_type_map ::
     B.CGType                   -- ^base type
  -> FP.FunPtr V.C_TypeMap2Fn   -- ^callback function
  -> FP.Ptr ()                  -- ^client data ptr
  -> FP.Ptr ()                  -- ^client data ptr
  -> IO (FP.Ptr ())

-- |Map a function over the all sub-types of the given base GType
--
typeMap :: B.GType              -- ^base type, for which all children will be mapped over
        -> V.TypeMap2Fn         -- ^function to be called for each child type
        -> FP.Ptr a             -- ^client data ptr
        -> FP.Ptr b             -- ^client data ptr
        -> IO (FP.Ptr ())
typeMap t cb p1 p2 = do
  let t' = B.gtypeToCGType t
  let p1' = FP.castPtr p1 :: FP.Ptr ()
  let p2' = FP.castPtr p2 :: FP.Ptr ()
  cb' <- V.mk_TypeMap2Fn $ V.wrap_TypeMap2Fn Nothing cb
  vips_type_map t' cb' p1' p2'

--
-- Vips argument mapping via FFI
--
data VipsOperationArgInfo = VipsOperationArgInfo
                            { ownedBy :: !V.Operation
                            , name :: !T.Text
                            , nickname :: !T.Text
                            , blurb :: !(Maybe T.Text)
                            , priority :: !Int32
                            , argumentFlags :: ![V.ArgumentFlags]
                            , argumentType :: !B.GType
                            , typename :: String
                            }
  deriving (Eq)

instance Show VipsOperationArgInfo where
  show (VipsOperationArgInfo _ name' nick' blurb' _ flags _ typename') =
    show name' ++ "/" ++ show nick' ++ ":" ++ show blurb' ++ " " ++ show flags ++ "-" ++ show typename'

-- |Get a list of VipsOperationArgInfo for all the known arguments
--  of the given operation.
listVipsOperationArgs :: V.Operation -> IO [VipsOperationArgInfo]
listVipsOperationArgs op = do
  args' <- newIORef ([] :: [VipsOperationArgInfo])
  argsPtr <- FSP.newStablePtr args'
  obj' <- V.toObject op
  _ <- argumentMap obj' listVipsOperationArg' (FSP.castStablePtrToPtr argsPtr) FP.nullPtr
  FSP.freeStablePtr argsPtr
  readIORef args'

-- |Add the VipsOperationArgInfo for an individual argument to the
--  list of all known arguments.
listVipsOperationArg' :: V.Object             -- ^the object to which this argument belongs
                      -> B.GParamSpec         -- ^the param spec for this argument
                      -> V.ArgumentClass      -- ^the argument class
                      -> V.ArgumentInstance   -- ^the argument instance
                      -> FP.Ptr ()            -- ^client data ptr; expected to be a StablePtr to an IORef [VipsOperationArgInfo]
                      -> FP.Ptr ()            -- ^client data ptr; ignored
                      -> IO (FP.Ptr ())
listVipsOperationArg' obj pspec cls inst a _ = do
  let pArgs = FSP.castPtrToStablePtr a :: FSP.StablePtr (IORef [VipsOperationArgInfo])
  args <- FSP.deRefStablePtr pArgs
  arg <- mkVipsOperationArgInfo' obj pspec cls inst
  modifyIORef' args (arg:)
  return FP.nullPtr

-- |Make a new VipsOperationArgInfo.
mkVipsOperationArgInfo' :: V.Object
                        -> B.GParamSpec
                        -> V.ArgumentClass
                        -> V.ArgumentInstance
                        -> IO VipsOperationArgInfo
mkVipsOperationArgInfo' obj pspec cls _ = do
  owner' <- V.unsafeCastTo V.Operation obj
  name' <- ParamSpec.paramSpecGetName pspec
  nickname' <- ParamSpec.paramSpecGetNick pspec
  blurb' <- ParamSpec.paramSpecGetBlurb pspec
  priority' <- V.getArgumentClassPriority cls
  gtype' <- GValue.gvalueType =<< ParamSpec.paramSpecGetDefaultValue pspec
  typename' <- V.gtypeName gtype'
  flags' <- V.getArgumentClassFlags cls
  return VipsOperationArgInfo
         { ownedBy = owner'
         , name = name'
         , nickname = nickname'
         , blurb = blurb'
         , priority = priority'
         , argumentFlags = flags'
         , argumentType = gtype'
         , typename = typename'
         }

foreign import ccall "vips_argument_map" vips_argument_map ::
     FP.Ptr V.Object             -- ^object whose args should be enumerated
  -> FP.FunPtr V.C_ArgumentMapFn -- ^function to be called for each argument
  -> FP.Ptr ()                   -- ^client data ptr
  -> FP.Ptr ()                   -- ^client data ptr
  -> IO (FP.Ptr ())

-- |Map a function over all the arguments for the given object.
--
argumentMap :: V.Object          -- ^object whose args should be enumerated
            -> V.ArgumentMapFn   -- ^function to be called for each argument
            -> FP.Ptr a          -- ^client data ptr
            -> FP.Ptr b          -- ^client data ptr
            -> IO (FP.Ptr ())
argumentMap obj cb p1 p2 = do
  let p1' = FP.castPtr p1 :: FP.Ptr ()
  let p2' = FP.castPtr p2 :: FP.Ptr ()
  cb' <- V.mk_ArgumentMapFn $ V.wrap_ArgumentMapFn Nothing cb
  FFP.withForeignPtr foreignPtr $ \fp ->
    vips_argument_map fp cb' p1' p2'
    where
      foreignPtr = V.managedForeignPtr . V.toManagedPtr $ obj
