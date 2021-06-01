{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (lines)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Test.Hspec

import           Vips
import qualified Vips as V (AutorotResult(..), PngloadBufferResult(..))
import qualified Vips.Arguments as Arg
import qualified GI.Vips as GV (Direction(..), arrayDoubleGet)
import qualified GI.Vips.Enums as GV (Angle(..))
import qualified GI.Vips.Flags as GV (ForeignFlags(..))
import qualified GI.Vips.Functions as GV (cacheSetMax)
import qualified GI.Vips.Objects.Image as GV (imageImageSetInt)

config :: VipsInit
config = VipsInit { progName = "test", checkLeaks = Default }

-- |If multiple tests perform the same operation, then libvips may
-- cache and reuse that operation. This muddies the waters for detecting
-- memory leaks.
-- Therefore, restrict the cache to hold only a single operation.
configureCache :: IO ()
configureCache = GV.cacheSetMax 1

main :: IO ()
main = withVips config . liftIO . hspec $ beforeAll_ configureCache $ do
  describe "simple image transform" $ do
    it "transforms an image without throwing an exception" $ example $ do
      buffer <- runVips $ vips (black 128 128) >>= vips . invert >>= vips . pngsaveBuffer
      runVips $ freeBlobBuffer buffer

  describe "image operation that returns a scalar" $ do
    it "performs an operation that returns a scalar" $ example $ do
      -- a 128x128 image transitioning from black (0) to white (255);
      -- this will therefore be counted as having one vertical 'line' at
      -- the point of transition from <128 to >= 128
      let gradientImg = grey 128 128 & Arg.uchar True
      lines <- runVips $ vips gradientImg >>= vips . countlines GV.DirectionVertical
      lines `shouldBe` 1

  describe "image operation that returns an array" $ do
    it "performs an operation that returns an array" $ example $ do
      -- a 2-band 32x32 image; elements in the first band have the value
      -- of their x-coordinate, elements in the second band have the value
      -- of their y-coordinate.
      let xyImg = xyz 32 32
      point <- runVips $ vips xyImg >>= vips . getpoint 5 2   -- N.B. x and y are reversed
      values <- GV.arrayDoubleGet point
      length values `shouldBe` 2
      values!!0 `shouldBe` (2 :: Double)
      values!!1 `shouldBe` (5 :: Double)
      runVips $ freeArrayDouble point

  describe "image operation that returns an enum" $ do
    it "performs an operation that returns an enum" $ example $ do
      xyImg <- runVips $ vips (xyz 32 32)
      GV.imageImageSetInt xyImg "orientation" 3
      result <- runVips $ vips . autorot $ xyImg
      let enumVal = V.angle result
      enumVal `shouldBe` GV.AngleD180

  describe "image operation that returns flags" $ do
    it "performs an operation that returns some flags" $ example $ do
      buffer <- runVips $ vips (black 128 128) >>= vips . pngsaveBuffer
      result <- runVips $ vips (pngloadBuffer buffer)
      let flags' = V.flags result
      flags' `shouldBe` GV.ForeignFlagsSequential
      runVips $ freeBlobBuffer buffer
      -- FIXME: this test leaks 1 x VipsArea
