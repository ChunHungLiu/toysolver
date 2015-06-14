{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ToySolver.Data.MIP
-- Copyright   :  (c) Masahiro Sakai 2011-2014
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Mixed-Integer Programming Problems with some commmonly used extensions
--
-----------------------------------------------------------------------------
module ToySolver.Data.MIP
  ( module ToySolver.Data.MIP.Base
  , readFile
  , readLPFile
  , readMPSFile
  , parseLPString
  , parseMPSString
  , writeFile
  , writeLPFile
  , writeMPSFile
  , toLPString
  , toMPSString
  ) where

import Prelude hiding (readFile, writeFile)
import qualified Prelude as P
import Data.Char
import System.FilePath (takeExtension)
import Text.Parsec

import ToySolver.Data.MIP.Base
import qualified ToySolver.Data.MIP.LPFile as LPFile
import qualified ToySolver.Data.MIP.MPSFile as MPSFile

-- | Parse .lp or .mps file based on file extension
readFile :: (IsVar v, RealFrac c) => FilePath -> IO (Either ParseError (Problem v c))
readFile fname =
  case map toLower (takeExtension fname) of
    ".lp"  -> readLPFile fname
    ".mps" -> readMPSFile fname
    ext -> ioError $ userError $ "unknown extension: " ++ ext

-- | Parse a file containing LP file data.
readLPFile :: (IsVar v, RealFrac c) => FilePath -> IO (Either ParseError (Problem v c))
readLPFile = LPFile.parseFile

-- | Parse a file containing MPS file data.
readMPSFile :: (IsVar v, RealFrac c) => FilePath -> IO (Either ParseError (Problem v c))
readMPSFile = MPSFile.parseFile

-- | Parse a string containing LP file data.
parseLPString :: (IsVar v, RealFrac c) => SourceName -> String -> Either ParseError (Problem v c)
parseLPString = LPFile.parseString

-- | Parse a string containing MPS file data.
parseMPSString :: (IsVar v, RealFrac c) => SourceName -> String -> Either ParseError (Problem v c)
parseMPSString = MPSFile.parseString

writeFile :: (IsVar v, RealFrac c) => FilePath -> Problem v c -> IO ()
writeFile fname prob =
  case map toLower (takeExtension fname) of
    ".lp"  -> writeLPFile fname prob
    ".mps" -> writeMPSFile fname prob
    ext -> ioError $ userError $ "unknown extension: " ++ ext

writeLPFile :: (IsVar v, RealFrac c) => FilePath -> Problem v c -> IO ()
writeLPFile fname prob =
  case LPFile.render prob of
    Left err -> ioError $ userError err
    Right s -> P.writeFile fname s

writeMPSFile :: (IsVar v, RealFrac c) => FilePath -> Problem v c -> IO ()
writeMPSFile fname prob = 
  case MPSFile.render prob of
    Left err -> ioError $ userError err
    Right s -> P.writeFile fname s

toLPString :: (IsVar v, RealFrac c) => Problem v c -> Either String String
toLPString = LPFile.render

toMPSString :: (IsVar v, RealFrac c) => Problem v c -> Either String String
toMPSString = MPSFile.render
