{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FourierMotzkin
-- Copyright   :  (c) Masahiro Sakai 2011
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (MultiParamTypeClasses, FunctionalDependencies)
--
-- Naïve implementation of Fourier-Motzkin Variable Elimination
-- 
-- Reference:
--
-- * <http://users.cecs.anu.edu.au/~michaeln/pubs/arithmetic-dps.pdf>
--
-----------------------------------------------------------------------------
module FourierMotzkin
    ( module Data.Expr
    , module Data.Formula
    , Lit (..)
    , eliminateQuantifiers
    , solve
    , solveConj

    -- FIXME
    , termR
    , Rat
    , collectBounds
    , boundConditions
    , evalBounds
    , simplify
    , constraintsToDNF
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.ArithRel
import Data.Expr
import Data.Formula
import Data.Linear
import qualified Data.LA as LA
import qualified Data.Interval as Interval

-- ---------------------------------------------------------------------------

type ExprZ = LA.Expr Integer

-- | (t,c) represents t/c, and c must be >0.
type Rat = (ExprZ, Integer)

evalRat :: Model Rational -> Rat -> Rational
evalRat model (e, d) = LA.lift1 1 (model IM.!) (LA.mapCoeff fromIntegral e) / (fromIntegral d)

-- | Literal
data Lit = Nonneg ExprZ | Pos ExprZ deriving (Show, Eq, Ord)

instance Variables Lit where
  vars (Pos t) = vars t
  vars (Nonneg t) = vars t

instance Complement Lit where
  notB (Pos t) = Nonneg (lnegate t)
  notB (Nonneg t) = Pos (lnegate t)

-- 制約集合の単純化
-- It returns Nothing when a inconsistency is detected.
simplify :: [Lit] -> Maybe [Lit]
simplify = fmap concat . mapM f
  where
    f :: Lit -> Maybe [Lit]
    f lit@(Pos e) =
      case LA.asConst e of
        Just x -> guard (x > 0) >> return []
        Nothing -> return [lit]
    f lit@(Nonneg e) =
      case LA.asConst e of
        Just x -> guard (x >= 0) >> return []
        Nothing -> return [lit]

-- ---------------------------------------------------------------------------

atomR :: RelOp -> Expr Rational -> Expr Rational -> Maybe (DNF Lit)
atomR op a b = do
  a' <- termR a
  b' <- termR b
  return $ case op of
    Le -> DNF [[a' `leR` b']]
    Lt -> DNF [[a' `ltR` b']]
    Ge -> DNF [[a' `geR` b']]
    Gt -> DNF [[a' `gtR` b']]
    Eql -> DNF [[a' `leR` b', a' `geR` b']]
    NEq -> DNF [[a' `ltR` b'], [a' `gtR` b']]

termR :: Expr Rational -> Maybe Rat
termR (Const c) = return (LA.constant (numerator c), denominator c)
termR (Var v) = return (LA.var v, 1)
termR (a :+: b) = do
  (t1,c1) <- termR a
  (t2,c2) <- termR b
  return (c2 .*. t1 .+. c1 .*. t2, c1*c2)
termR (a :*: b) = do
  (t1,c1) <- termR a
  (t2,c2) <- termR b
  msum [ do{ c <- LA.asConst t1; return (c .*. t2, c1*c2) }
       , do{ c <- LA.asConst t2; return (c .*. t1, c1*c2) }
       ]
termR (a :/: b) = do
  (t1,c1) <- termR a
  (t2,c2) <- termR b
  c3 <- LA.asConst t2
  guard $ c3 /= 0
  return (c2 .*. t1, c1*c3)

leR, ltR, geR, gtR :: Rat -> Rat -> Lit
leR (e1,c) (e2,d) = Nonneg $ normalizeExprR $ c .*. e2 .-. d .*. e1
ltR (e1,c) (e2,d) = Pos $ normalizeExprR $ c .*. e2 .-. d .*. e1
geR = flip leR
gtR = flip gtR

normalizeExprR :: ExprZ -> ExprZ
normalizeExprR e = LA.mapCoeff (`div` d) e
  where d = abs $ gcd' $ map fst $ LA.terms e

-- ---------------------------------------------------------------------------

{-
(ls1,ls2,us1,us2) represents
{ x | ∀(M,c)∈ls1. M/c≤x, ∀(M,c)∈ls2. M/c<x, ∀(M,c)∈us1. x≤M/c, ∀(M,c)∈us2. x<M/c }
-}
type BoundsR = ([Rat], [Rat], [Rat], [Rat])

eliminate :: Var -> [Lit] -> DNF Lit
eliminate v xs = DNF [rest] .&&. boundConditions bnd
  where
    (bnd, rest) = collectBounds v xs

collectBounds :: Var -> [Lit] -> (BoundsR, [Lit])
collectBounds v = foldr phi (([],[],[],[]),[])
  where
    phi :: Lit -> (BoundsR, [Lit]) -> (BoundsR, [Lit])
    phi lit@(Nonneg t) x = f False lit t x
    phi lit@(Pos t) x = f True lit t x

    f :: Bool -> Lit -> ExprZ -> (BoundsR, [Lit]) -> (BoundsR, [Lit])
    f strict lit t (bnd@(ls1,ls2,us1,us2), xs) =
      case LA.extract v t of
        (c,t') ->
          case c `compare` 0 of
            EQ -> (bnd, lit : xs)
            GT ->
              if strict
              then ((ls1, (lnegate t', c) : ls2, us1, us2), xs) -- 0 < cx + M ⇔ -M/c <  x
              else (((lnegate t', c) : ls1, ls2, us1, us2), xs) -- 0 ≤ cx + M ⇔ -M/c ≤ x
            LT ->
              if strict
              then ((ls1, ls2, us1, (t', negate c) : us2), xs) -- 0 < cx + M ⇔ x < M/-c
              else ((ls1, ls2, (t', negate c) : us1, us2), xs) -- 0 ≤ cx + M ⇔ x ≤ M/-c

boundConditions :: BoundsR -> DNF Lit
boundConditions  (ls1, ls2, us1, us2) = DNF $ maybeToList $ simplify $ 
  [ x `leR` y | x <- ls1, y <- us1 ] ++
  [ x `ltR` y | x <- ls1, y <- us2 ] ++ 
  [ x `ltR` y | x <- ls2, y <- us1 ] ++
  [ x `ltR` y | x <- ls2, y <- us2 ]

eliminateQuantifiers :: Formula (Atom Rational) -> Maybe (DNF Lit)
eliminateQuantifiers = f
  where
    f T = return true
    f F = return false
    f (Atom (Rel a op b)) = atomR op a b
    f (And a b) = liftM2 (.&&.) (f a) (f b)
    f (Or a b) = liftM2 (.||.) (f a) (f b)
    f (Not a) = f (pushNot a)
    f (Imply a b) = f (Or (Not a) b)
    f (Equiv a b) = f (And (Imply a b) (Imply b a))
    f (Forall v a) = do
      dnf <- f (Exists v (pushNot a))
      return (notB dnf)
    f (Exists v a) = do
      dnf <- f a
      return $ orB [eliminate v xs | xs <- unDNF dnf]

solve :: Formula (Atom Rational) -> SatResult Rational
solve formula =
  case eliminateQuantifiers formula of
    Nothing -> Unknown
    Just dnf ->
      case msum [solve' vs xs | xs <- unDNF dnf] of
        Nothing -> Unsat
        Just m -> Sat m
  where
    vs = IS.toList (vars formula)

solve' :: [Var] -> [Lit] -> Maybe (Model Rational)
solve' vs2 xs = simplify xs >>= go vs2
  where
    go [] [] = return IM.empty
    go [] _ = mzero
    go (v:vs) ys = msum (map f (unDNF (boundConditions bnd)))
      where
        (bnd, rest) = collectBounds v ys
        f zs = do
          model <- go vs (zs ++ rest)
          val <- Interval.pickup (evalBounds model bnd)
          return $ IM.insert v val model

evalBounds :: Model Rational -> BoundsR -> Interval.Interval Rational
evalBounds model (ls1,ls2,us1,us2) =
  foldl' Interval.intersection Interval.univ $ 
    [ Interval.interval (Just (True, evalRat model x)) Nothing  | x <- ls1 ] ++
    [ Interval.interval (Just (False, evalRat model x)) Nothing | x <- ls2 ] ++
    [ Interval.interval Nothing (Just (True, evalRat model x))  | x <- us1 ] ++
    [ Interval.interval Nothing (Just (False, evalRat model x)) | x <- us2 ]

solveConj :: [LA.Atom Rational] -> Maybe (Model Rational)
solveConj cs = msum [solve' vs cs2 | cs2 <- unDNF (constraintsToDNF cs)]
  where
    vs = IS.toList (vars cs)

-- ---------------------------------------------------------------------------

constraintsToDNF :: [LA.Atom Rational] -> DNF Lit
constraintsToDNF = andB . map constraintToDNF

constraintToDNF :: LA.Atom Rational -> DNF Lit
constraintToDNF (Rel lhs op rhs) = DNF $
  case op of
    Eql -> [[Nonneg lhs', Nonneg (lnegate lhs')]]
    NEq -> [[Pos lhs'], [Pos (lnegate lhs')]]
    Ge  -> [[Nonneg lhs']]
    Le  -> [[Nonneg (lnegate lhs')]]
    Gt  -> [[Pos lhs']]
    Lt  -> [[Pos (lnegate lhs')]]
  where
    lhs' = normalize (lhs .-. rhs)

    normalize :: LA.Expr Rational -> ExprZ
    normalize e = LA.mapCoeff (round . (*c)) e
      where
        c = fromIntegral $ foldl' lcm 1 ds
        ds = [denominator c | (c,_) <- LA.terms e]

-- ---------------------------------------------------------------------------

gcd' :: [Integer] -> Integer
gcd' [] = 1
gcd' xs = foldl1' gcd xs

-- ---------------------------------------------------------------------------
