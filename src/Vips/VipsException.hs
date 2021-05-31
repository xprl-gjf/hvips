
-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.VipsException
  ( VipsException(..)
  ) where

import           Control.Exception
import qualified Data.Text as T

newtype VipsException = VipsException { msg :: T.Text }
  deriving (Show)
instance Exception VipsException
