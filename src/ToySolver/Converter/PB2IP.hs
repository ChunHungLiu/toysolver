{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ToySolver.Converter.PB2IP
-- Copyright   :  (c) Masahiro Sakai 2011-2014
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module ToySolver.Converter.PB2IP
  ( convert
  , convertWBO
  ) where

import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String

import qualified Data.PseudoBoolean as PBFile
import qualified ToySolver.Data.MIP as MIP
import qualified ToySolver.SAT.Types as SAT

convert :: MIP.IsVar v => PBFile.Formula -> (MIP.Problem v Rational, Map v Rational -> SAT.Model)
convert formula = (mip, mtrans (PBFile.pbNumVars formula))
  where
    mip = MIP.Problem
      { MIP.dir = dir
      , MIP.objectiveFunction = (Nothing, obj2)
      , MIP.constraints = cs2
      , MIP.sosConstraints = []
      , MIP.userCuts = []
      , MIP.varInfo = Map.fromList
          [ ( v
            , MIP.VarInfo
              { MIP.varType   = MIP.IntegerVariable
              , MIP.varBounds = (0, 1)
              }
            )
          | v <- vs
          ]
      }

    vs = [convVar v | v <- [1..PBFile.pbNumVars formula]]

    (dir,obj2) =
      case PBFile.pbObjectiveFunction formula of
        Just obj' -> (MIP.OptMin, convExpr obj')
        Nothing   -> (MIP.OptMin, convExpr [])

    cs2 = do
      (lhs,op,rhs) <- PBFile.pbConstraints formula
      let op2 = case op of
                  PBFile.Ge -> MIP.Ge
                  PBFile.Eq -> MIP.Eql
          lhs2 = convExpr lhs
          lhs3a = [t | t@(MIP.Term _ (_:_)) <- lhs2]
          lhs3b = sum [c | MIP.Term c [] <- lhs2]
      return $ MIP.Constraint
        { MIP.constrLabel     = Nothing
        , MIP.constrIndicator = Nothing
        , MIP.constrIsLazy    = False
        , MIP.constrBody      = (lhs3a, op2, fromIntegral rhs - lhs3b)
        }

convExpr :: forall v. MIP.IsVar v => PBFile.Sum -> MIP.Expr v Rational
convExpr s = concatMap g2 s
  where
    g2 :: PBFile.WeightedTerm -> MIP.Expr v Rational
    g2 (w, tm) = foldl' prodE [MIP.Term (fromIntegral w) []] (map g3 tm)

    g3 :: PBFile.Lit -> MIP.Expr v Rational
    g3 x
      | x > 0     = [MIP.Term 1 [convVar x]]
      | otherwise = [MIP.Term 1 [], MIP.Term (-1) [convVar (abs x)]]

    prodE :: MIP.Expr v Rational -> MIP.Expr v Rational -> MIP.Expr v Rational
    prodE e1 e2 = [prodT t1 t2 | t1 <- e1, t2 <- e2]

    prodT :: MIP.Term v Rational -> MIP.Term v Rational -> MIP.Term v Rational
    prodT (MIP.Term c1 vs1) (MIP.Term c2 vs2) = MIP.Term (c1*c2) (vs1++vs2)

convVar :: MIP.IsVar v => PBFile.Var -> v
convVar x = fromString ("x" ++ show x)

convertWBO :: forall v. MIP.IsVar v => Bool -> PBFile.SoftFormula -> (MIP.Problem v Rational, Map v Rational -> SAT.Model)
convertWBO useIndicator formula = (mip, mtrans (PBFile.wboNumVars formula))
  where
    mip = MIP.Problem
      { MIP.dir = MIP.OptMin
      , MIP.objectiveFunction = (Nothing, obj2)
      , MIP.constraints = topConstr ++ map snd cs2
      , MIP.sosConstraints = []
      , MIP.userCuts = []
      , MIP.varInfo = Map.fromList
          [ ( v
            , MIP.VarInfo
              { MIP.varType   = MIP.IntegerVariable
              , MIP.varBounds = (0, 1)
              }
            )
          | v <- vs
          ]
      }

    vs = [convVar v | v <- [1..PBFile.wboNumVars formula]] ++ [v | (ts, _) <- cs2, (_, v) <- ts]

    obj2 = [MIP.Term (fromIntegral w) [v] | (ts, _) <- cs2, (w, v) <- ts]

    topConstr :: [MIP.Constraint v Rational]
    topConstr = 
     case PBFile.wboTopCost formula of
       Nothing -> []
       Just t ->
          [ MIP.Constraint
            { MIP.constrLabel     = Nothing
            , MIP.constrIndicator = Nothing
            , MIP.constrIsLazy    = False
            , MIP.constrBody      = (obj2, MIP.Le, fromInteger t - 1)
            }
          ]

    cs2 :: [([(Integer, v)], MIP.Constraint v Rational)]
    cs2 = do
      (n, (w, (lhs,op,rhs))) <- zip [(0::Int)..] (PBFile.wboConstraints formula)
      let 
          lhs2 = convExpr lhs
          lhs3 = [t | t@(MIP.Term _ (_:_)) <- lhs2]
          rhs3 = fromIntegral rhs - sum [c | MIP.Term c [] <- lhs2]
          v = fromString ("r" ++ show n)
          (ts,ind) =
            case w of
              Nothing -> ([], Nothing)
              Just w2 -> ([(w2,v)], Just (v,0))
      if isNothing w || useIndicator then do
         let op2 =
               case op of
                 PBFile.Ge -> MIP.Ge
                 PBFile.Eq -> MIP.Eql
             c = MIP.Constraint
                 { MIP.constrLabel     = Nothing
                 , MIP.constrIndicator = ind
                 , MIP.constrIsLazy    = False
                 , MIP.constrBody      = (lhs3, op2, rhs3)
                 }
         return (ts, c)
       else do
         let (lhsGE,rhsGE) = relaxGE v (lhs3,rhs3)
             c1 = MIP.Constraint
                  { MIP.constrLabel     = Nothing
                  , MIP.constrIndicator = Nothing
                  , MIP.constrIsLazy    = False
                  , MIP.constrBody      = (lhsGE, MIP.Ge, rhsGE)
                  }
         case op of
           PBFile.Ge -> do
             return (ts, c1)
           PBFile.Eq -> do
             let (lhsLE,rhsLE) = relaxLE v (lhs3,rhs3)
                 c2 = MIP.Constraint
                      { MIP.constrLabel     = Nothing
                      , MIP.constrIndicator = Nothing
                      , MIP.constrIsLazy    = False
                      , MIP.constrBody      = (lhsLE, MIP.Le, rhsLE)
                      }
             [ (ts, c1), ([], c2) ]

relaxGE :: MIP.IsVar v => v -> (MIP.Expr v Rational, Rational) -> (MIP.Expr v Rational, Rational)
relaxGE v (lhs, rhs) = (MIP.Term (rhs - lhs_lb) [v] : lhs, rhs)
  where
    lhs_lb = sum [min c 0 | MIP.Term c _ <- lhs]

relaxLE :: MIP.IsVar v => v -> (MIP.Expr v Rational, Rational) -> (MIP.Expr v Rational, Rational)
relaxLE v (lhs, rhs) = (MIP.Term (rhs - lhs_ub) [v] : lhs, rhs)
  where
    lhs_ub = sum [max c 0 | MIP.Term c _ <- lhs]

mtrans :: MIP.IsVar v => Int -> Map v Rational -> SAT.Model
mtrans nvar m =
  array (1, nvar)
    [ (i, val)
    | i <- [1 .. nvar]
    , let val =
            case Map.findWithDefault 0 (convVar i) m of
              0  -> False
              1  -> True
              v0 -> error (show v0 ++ " is neither 0 nor 1")
    ]
