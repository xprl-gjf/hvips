{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             GeneralizedNewtypeDeriving #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.VipsIO
  ( VipsInit(..)
  , VipsCheckLeaks(..)
  , VipsIO(..)
  , withVips
  ) where

import           Control.Applicative (liftA2)
import           Control.Exception (finally)
import           Control.Monad.Catch (MonadThrow)
import qualified Data.Text as T
import qualified Data.GI.Base.ShortPrelude as SP (MonadIO, liftIO)

import           Vips.Internal.VipsCResult
import qualified GI.Vips as V

--
-- vipslib initialization and VipsIO monad
--

-- | config setting for libvips object leak checking
data VipsCheckLeaks = Enabled       -- ^explicitly enable vips object leak checking
                    | Default       -- ^inherit vips object leak checking from the VIPS_LEAK env var

-- | libvips initialization parameters.
data VipsInit = VipsInit
  { checkLeaks  :: !VipsCheckLeaks  -- ^enable or disable vips object leak checking
  , progName    :: !T.Text          -- ^the program name passed to libvips as argv[0]
  }

-- | The VipsIO monad, representing a context having an
-- | initialized libvips environment within the IO monad.
newtype VipsIO a = VipsIO { runVips :: IO a }
  deriving (Functor, Applicative, Monad, SP.MonadIO, MonadThrow)

instance (Semigroup a) => Semigroup (VipsIO a) where
  (<>) = liftA2(<>)
instance (Monoid a) => Monoid (VipsIO a) where
  mempty = pure mempty
  mappend = (<>)

-- | Run in IO the action that requires an initialized
-- | libvips environment
withVips :: (SP.MonadIO m) => VipsInit -> VipsIO a -> m a
withVips x f = SP.liftIO $ do
  init' x
  flip finally shutdown' $ runVips f

init' :: VipsInit -> IO ()
init' VipsInit{..} = do
  _ <- V.init progName <|> error "Failed to initialise libvips"
  leakSet' checkLeaks
  where
    leakSet' Enabled = V.leakSet True
    leakSet' Default = return ()

shutdown' :: IO ()
shutdown' = V.shutdown
