{-# LANGUAGE DataKinds,
             MultiParamTypeClasses,
             FlexibleContexts #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Arguments where

import qualified GI.Vips as GV
import           Vips.Introspection.Operations

--
-- Vips named arguments:
--

-- Note:
-- "img" is used as a synonym for "in",
-- "imgs" is used as a synonym for "in" for an array of images
-- since `in` is a Haskell reserved word
