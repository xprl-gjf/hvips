{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  LGPL-2.1

module Vips
  ( VipsInit(..)
  , VipsIO(..)
  , withVips
  , loadImage
  , saveImage
  , invert
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Catch
import qualified Data.Text as T
import Data.GI.Base.ShortPrelude (MonadIO, liftIO)
import Data.GI.Base.GValue (GValue, IsGValue)
import qualified Data.GI.Base.GValue as GValue (toGValue, fromGValue, unsetGValue)
import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified GI.GObject.Objects.Object as GObject

import Vips.VipsResult
import qualified GI.Vips as V

--
-- vipslib initialization and VipsIO monad
--

-- | libvips initialization parameters.
data VipsInit = VipsInit
  { checkLeaks  :: !Bool      -- ^enable or disable vips object leak checking
  , progName    :: !T.Text    -- ^the program name passed to libvips as argv[0]
  }

-- | The VipsIO monad, representing a context having an
-- | initialized libvips environment within the IO monad.
newtype VipsIO a = VipsIO { unVips :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance (Semigroup a) => Semigroup (VipsIO a) where
  (<>) = liftA2(<>)
instance (Monoid a) => Monoid (VipsIO a) where
  mempty = pure mempty
  mappend = (<>)

-- | Run in IO the action that requires an initialized
-- | libvips environment
withVips :: (MonadIO m) => VipsInit -> VipsIO a -> m a
withVips VipsInit{..} f = liftIO $ do
  _ <- V.init progName <|> error "Failed to initialise libvips"
  V.leakSet checkLeaks
  result <- unVips f
  V.shutdown
  return result


--
-- Vips image operations:
--
loadImage :: FilePath -> VipsIO (Maybe V.Image)
loadImage f = do
  result <- callVips args =<< V.foreignFindLoad f'
    <|> error ("Failed to load image \"" <> f <> "\": unsupported image format.")
  imgOut <- V.imageNew
  let out = VArg "out" $ VImage imgOut
  result' <- getOutput out result
  clearArg out
  return result'
  where
    f' = T.pack f
    args = [VArg "filename" $ VText f']


invert :: Maybe V.Image -> VipsIO (Maybe V.Image)
invert Nothing  = return Nothing
invert (Just i) = do
  result <- callVips args "invert"
  let out = VArg "out" $ VImage i
  getOutput out result
  where
    args = [VArg "in" $ VImage i]


saveImage :: FilePath -> Maybe V.Image -> VipsIO ()
saveImage _ Nothing = return ()
saveImage f (Just i) = do
  result <- callVips args =<< V.foreignFindSave f'
    <|> error ("Unable to save image \"" <> f <> "\": unsupported image format.")
  getNone result
  where
    f' = T.pack f
    args = [ VArg "in" $ VImage i
           , VArg "filename" $ VText f'
           ]

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
