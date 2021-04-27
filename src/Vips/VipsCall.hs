{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  LGPL-2.1

module Vips.VipsCall where

import qualified Data.Text as T
import Data.GI.Base.ShortPrelude (liftIO)
import Data.GI.Base.GValue (GValue, IsGValue)
import qualified Data.GI.Base.GValue as GValue (toGValue, fromGValue, unsetGValue)
import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified GI.GObject.Objects.Object as GObject

import Vips.VipsIO
import qualified GI.Vips as V

--
-- Vips operations FFI via GObject introspection
--

-- | TODO: consider using Symbol?
type ArgName = T.Text

data VArgValue = VText  T.Text
               | VImage V.Image

data VArg = VArg ArgName VArgValue

argName :: VArg -> ArgName
argName (VArg a _) = a

argGValue :: VArg -> IO GValue
argGValue (VArg _ (VText t))  = GValue.toGValue (Just t)
argGValue (VArg _ (VImage i)) = GValue.toGValue (Just i)

clearArg :: VArg -> VipsIO ()
clearArg (VArg _ (VImage i)) = liftIO $ GObject.objectUnref i
clearArg _                   = liftIO $ return ()

callVips :: [VArg] -> T.Text -> VipsIO V.Operation
callVips args name =
  runVips args =<< V.operationNew name

runVips :: [VArg] -> V.Operation -> VipsIO V.Operation
runVips args op = do
  setProperties args op
  result <- V.cacheOperationBuild op
  liftIO $ clearOp op
  sequence_ $ clearArg <$> args
  return result

setProperties :: [VArg] -> V.Operation -> VipsIO ()
setProperties args op = liftIO $
  sequence_ $ setProperty <$> args
  where
    setProperty a =
      GObject.objectSetProperty op (argName a) =<< argGValue a

getOutput :: (IsGValue result) => VArg -> V.Operation -> VipsIO result
getOutput arg op = liftIO $ argGValue arg >>= \v -> do
  GObject.objectGetProperty op (argName arg) v
  clearOp op
  result <- GValue.fromGValue v
  B.ManagedPtr.withManagedPtr v GValue.unsetGValue
  return result

getNone :: V.Operation -> VipsIO ()
getNone = liftIO . clearOp

clearOp :: V.Operation -> IO ()
clearOp op = do
  V.objectUnrefOutputs op
  GObject.objectUnref op
