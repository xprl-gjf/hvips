{-# LANGUAGE OverloadedStrings, RecordWildCards,
             GeneralizedNewtypeDeriving #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.VipsIO
  ( VipsInit(..)
  , VipsIO(..)
  , withVips
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad.Catch (MonadThrow)
import qualified Data.Text as T
import qualified Data.GI.Base.ShortPrelude as SP (MonadIO, liftIO)

import           Vips.Internal.VipsResult
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
  deriving (Functor, Applicative, Monad, SP.MonadIO, MonadThrow)

instance (Semigroup a) => Semigroup (VipsIO a) where
  (<>) = liftA2(<>)
instance (Monoid a) => Monoid (VipsIO a) where
  mempty = pure mempty
  mappend = (<>)

-- | Run in IO the action that requires an initialized
-- | libvips environment
withVips :: (SP.MonadIO m) => VipsInit -> VipsIO a -> m a
withVips VipsInit{..} f = SP.liftIO $ do
  _ <- V.init progName <|> error "Failed to initialise libvips"
  V.leakSet checkLeaks
  result <- unVips f
  V.shutdown
  return result
