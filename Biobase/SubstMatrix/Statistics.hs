
-- | This module provides statistics needed for substitution matrices. It is a
-- very modest attempt to replicate some of the Blast statistics.

module Biobase.SubstMatrix.Statistics where

import Data.Vector.Unboxed (Unbox)
import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Monadic as SM
import Debug.Trace

import Data.PrimitiveArray as PA
import           Biobase.Primary.Letter

import Biobase.SubstMatrix.Types



-- | estimate Blast lambda.
--
-- TODO use ExceptT

estimateLambda
  :: (Unbox s, Num s, Real s)
  => AASubstMat t s a b -> Double
{-# Inlinable estimateLambda #-}
estimateLambda (AASubstMat mat _) = go 1000 1 2 0 where
  go count lambda high low
    | count <= 0 = error "no convergence?!"
    | (high-low) <= 0.001 = lambda
    | s >  1              = go (count-1) ((lambda+low)/2)  lambda low
    | s <= 1              = go (count-1) ((lambda+high)/2) high   lambda
    where
      -- get the rows and columns, needed to get the probs for each row/column right.
      (ZZ:..r':..c') = PA.upperBound mat
      r = fromRational $ toRational $ size r'
      c = fromRational $ toRational $ size c'
      -- sum of all scores
      s = unId . SM.foldl' (+) 0 $ SM.map eachElem $ PA.assocsS mat
      eachElem (Z:.i:.j, z) = (1/r) * (1/c) * exp (lambda * (fromRational $ toRational z))

