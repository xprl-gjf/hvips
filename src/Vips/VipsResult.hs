{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  LGPL-2.1

module Vips.VipsResult where

import Data.Int
import qualified Data.Text as T
import Data.GI.Base.ShortPrelude

-- |Convenient class for evaluating success or failure
-- |of libvips C function calls.
class VipsResult a where
  isOK :: a -> Bool

instance VipsResult Bool where
  isOK a = a

instance VipsResult Int32 where
  isOK a = a == 0

instance VipsResult T.Text where
  isOK a = a /= T.pack "P.Nothing"

-- |Evaluate success of a libvips action;
-- |return the result if the action was successful,
-- |else throw an error.
(<|>) :: (Monad m, VipsResult a) =>  m a -> a -> m a
a <|> err = result <$> a
  where
    result x = if isOK x then x else err
