{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ToySolver.Data.MIP.Base
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
module ToySolver.Data.MIP.Base
  ( Problem (..)
  , IsVar (..)
  , Expr
  , Term (..)
  , OptDir (..)
  , ObjectiveFunction
  , Constraint (..)
  , Bounds
  , VarType (..)
  , VarInfo (..)
  , BoundExpr
  , Extended (..)
  , RelOp (..)
  , SOSType (..)
  , SOSConstraint (..)
  , defaultBounds
  , defaultLB
  , defaultUB
  , getVarInfo
  , getVarType
  , getBounds
  , variables
  , integerVariables
  , semiContinuousVariables
  , semiIntegerVariables

  -- * Utilities
  , Variables (..)
  , intersectBounds
  ) where

import Data.Default.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.ExtendedReal
import Data.OptDir

import Data.Interned
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Interned.String as IS
import qualified Data.Interned.Text as IT
import qualified Data.Interned.ByteString as IBS

-- ---------------------------------------------------------------------------

-- | Problem
data Problem v c
  = Problem
  { dir :: OptDir
  , objectiveFunction :: ObjectiveFunction v c
  , constraints :: [Constraint v c]
  , sosConstraints :: [SOSConstraint v c]
  , userCuts :: [Constraint v c]
  , varInfo :: Map v (VarInfo c)
  }
  deriving (Show, Eq, Ord)

instance Default (Problem v c) where
  def = Problem
        { dir = OptMin
        , objectiveFunction = (Nothing, [])
        , constraints = []
        , sosConstraints = []
        , userCuts = []
        , varInfo = Map.empty
        }

class (Ord v, IsString v) => IsVar v where
  fromVar :: v -> String
  
instance IsVar String where
  fromVar = id
  
instance IsVar BS.ByteString where
  fromVar = BS.unpack
  
instance IsVar T.Text where
  fromVar = T.unpack

instance IsVar IS.InternedString where
  fromVar = unintern

instance IsVar IBS.InternedByteString where
  fromVar = fromVar . unintern
  
instance IsVar IT.InternedText where
  fromVar = fromVar . unintern

-- | expressions
type Expr v c = [Term v c]

-- | terms
data Term v c = Term c [v]
  deriving (Eq, Ord, Show)

-- | objective function
type ObjectiveFunction v c = (Maybe v, Expr v c)

-- | constraint
data Constraint v c
  = Constraint
  { constrLabel     :: Maybe v
  , constrIndicator :: Maybe (v, c)
  , constrBody      :: (Expr v c, RelOp, c)
  , constrIsLazy    :: Bool
  }
  deriving (Eq, Ord, Show)

instance Num c => Default (Constraint v c) where
  def = Constraint
        { constrLabel = Nothing
        , constrIndicator = Nothing
        , constrBody = ([], Le, 0)
        , constrIsLazy = False
        }

data VarType
  = ContinuousVariable
  | IntegerVariable
-- 'nothaddock' is inserted not to confuse haddock
  -- nothaddock | BinaryVariable
  | SemiContinuousVariable
  | SemiIntegerVariable
  deriving (Eq, Ord, Show)

instance Default VarType where
  def = ContinuousVariable

data VarInfo c
  = VarInfo
  { varType   :: VarType
  , varBounds :: Bounds c
  }
 deriving (Eq, Ord, Show)

instance Real c => Default (VarInfo c) where
  def = defaultVarInfo

defaultVarInfo :: Real c => VarInfo c
defaultVarInfo
  = VarInfo
  { varType   = ContinuousVariable
  , varBounds = defaultBounds
  }

-- | type for representing lower/upper bound of variables
type Bounds c = (BoundExpr c, BoundExpr c)

-- | type for representing lower/upper bound of variables
type BoundExpr c = Extended c

-- | relational operators
data RelOp = Le | Ge | Eql
    deriving (Eq, Ord, Enum, Show)

-- | types of SOS (special ordered sets) constraints
data SOSType
  = S1 -- ^ Type 1 SOS constraint
  | S2 -- ^ Type 2 SOS constraint
    deriving (Eq, Ord, Enum, Show, Read)

-- | SOS (special ordered sets) constraints
data SOSConstraint v c
  = SOSConstraint
  { sosLabel :: Maybe v
  , sosType  :: SOSType
  , sosBody  :: [(v, c)]
  }
  deriving (Eq, Ord, Show)

class IsVar v => Variables v a where
  vars :: a -> Set v

instance Variables v a => Variables v [a] where
  vars = Set.unions . map vars

instance (Variables v a, Variables v b) => Variables v (Either a b) where
  vars (Left a)  = vars a
  vars (Right b) = vars b

instance IsVar v => Variables v (Problem v c) where
  vars = variables

instance IsVar v => Variables v (Term v c) where
  vars (Term _ xs) = Set.fromList xs

instance IsVar v => Variables v (Constraint v c) where
  vars Constraint{ constrIndicator = ind, constrBody = (lhs, _, _) } =
    vars lhs `Set.union` vs2
    where
      vs2 = maybe Set.empty (Set.singleton . fst) ind

instance IsVar v => Variables v (SOSConstraint v c) where
  vars SOSConstraint{ sosBody = xs } = Set.fromList (map fst xs)

-- | default bounds
defaultBounds :: Real c => Bounds c
defaultBounds = (defaultLB, defaultUB)

-- | default lower bound (0)
defaultLB :: Real c => BoundExpr c
defaultLB = 0

-- | default upper bound (+∞)
defaultUB :: Real c => BoundExpr c
defaultUB = PosInf

-- | looking up attributes for a variable
getVarInfo :: (Ord v, Real c) => Problem v c -> v -> VarInfo c
getVarInfo lp v = Map.findWithDefault defaultVarInfo v (varInfo lp)

-- | looking up bounds for a variable
getVarType :: (Ord v, Real c) => Problem v c -> v -> VarType
getVarType lp v = varType $ getVarInfo lp v

-- | looking up bounds for a variable
getBounds :: (Ord v, Real c) => Problem v c -> v -> Bounds c
getBounds lp v = varBounds $ getVarInfo lp v

intersectBounds :: Ord c => Bounds c -> Bounds c -> Bounds c
intersectBounds (lb1,ub1) (lb2,ub2) = (max lb1 lb2, min ub1 ub2)

variables :: IsVar v => Problem v c -> Set v
variables lp = Map.keysSet $ varInfo lp

integerVariables :: IsVar v => Problem v c -> Set v
integerVariables lp = Map.keysSet $ Map.filter p (varInfo lp)
  where
    p VarInfo{ varType = vt } = vt == IntegerVariable

semiContinuousVariables :: IsVar v => Problem v c -> Set v
semiContinuousVariables lp = Map.keysSet $ Map.filter p (varInfo lp)
  where
    p VarInfo{ varType = vt } = vt == SemiContinuousVariable

semiIntegerVariables :: IsVar v => Problem v c -> Set v
semiIntegerVariables lp = Map.keysSet $ Map.filter p (varInfo lp)
  where
    p VarInfo{ varType = vt } = vt == SemiIntegerVariable
