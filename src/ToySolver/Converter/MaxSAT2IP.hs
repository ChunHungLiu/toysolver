{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ToySolver.Converter.MaxSAT2IP
-- Copyright   :  (c) Masahiro Sakai 2011-2014
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module ToySolver.Converter.MaxSAT2IP
  ( convert
  ) where

import Data.Map (Map)
import qualified ToySolver.Data.MIP as MIP
import qualified ToySolver.Text.MaxSAT as MaxSAT
import ToySolver.SAT.Types
import qualified ToySolver.Converter.MaxSAT2WBO as MaxSAT2WBO
import qualified ToySolver.Converter.PB2IP as PB2IP

convert :: MIP.IsVar v => Bool -> MaxSAT.WCNF -> (MIP.Problem v Rational, Map v Rational -> Model)
convert useIndicator wcnf = PB2IP.convertWBO useIndicator (MaxSAT2WBO.convert wcnf)
