{-# LANGUAGE OverloadedStrings,
             RecordWildCards, NamedFieldPuns #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Main where

import qualified Data.Text as T
import Control.Monad
import Options.Applicative
import System.Environment (getProgName)

import Vips hiding (switch)


-- |Command line arguments
data Args = Args
  { checkLeaks :: !VipsCheckLeaks   -- ^Enable vips GObject reference leak checking
  , inFile     :: !FilePath         -- ^Input file path, possibly with load options appended
                                    --  e.g. "/path/to/image.jpg[Q=90,strip]"
                                    --  Image image type is deduced by libvips sniffing the first few bytes.
  , outFile    :: !FilePath         -- ^Output file path.
                                    --  Output image type is determined by the filename suffix.
  }

-- |Parser for command line arguments
argParse :: Parser Args
argParse = Args
  <$> flag Default Enabled ( long "check-mem" <> short 'c' <> help "Enable vips memory leak checks" )
  <*> argument str (metavar "INPUT")
  <*> argument str (metavar "OUTPUT")

main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info (argParse <**> helper)
      (  fullDesc
      <> progDesc "Process an input image and save as output"
      <> header "hvips-exe - sample app for haskell libvips bindings" )

-- |Process the images specified by the command line args,
-- |within the context of an initialized libvips env
runApp :: Args -> IO ()
runApp Args{..} = do
  progName <- T.pack <$> getProgName
  withVips VipsInit { progName, checkLeaks } $
    processImage inFile outFile

-- ===========================
-- the magic happens here...:

-- |The applied image transformation;
-- |saves an inverted, blurred copy of the source image
processImage :: FilePath -> FilePath -> VipsIO ()
processImage inFile outFile = void . runVips $
      vips loadImage inFile
  >>= vips blur
  >>= vips invert
  >>= vips (saveImage outFile)
    where
      blur = gaussblur 1.2 <&> minAmpl' (0.025 :: Double)

-- ===========================
