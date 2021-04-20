{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  LGPL-2.1

module Main where

import qualified Data.Text as T
import Options.Applicative
import System.Environment (getProgName)

import Vips

-- |Command line arguments
data Args = Args
  { checkLeaks :: !Bool         -- ^Enable vips GObject reference leak checking
  , inFile     :: !FilePath     -- ^Input file path, possibly with load options appended
                                --  e.g. "/path/to/image.jpg[Q=90,strip]"
                                --  Image image type is deduced by libvips sniffing the first few bytes.
  , outFile    :: !FilePath     -- ^Output file path.
                                --  Output image type is determined by the filename suffix.
  }

-- |Parser for command line arguments
argParse :: Parser Args
argParse = Args
  <$> switch ( long "check-mem" <> short 'c' <> help "Enable vips memory leak checks" )
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

-- |The applied image transformation;
-- |saves an inverted copy of the source image
processImage :: FilePath -> FilePath -> VipsIO ()
processImage inFile outFile =
  loadImage inFile >>= invert >>= saveImage outFile
