
-- | This module provides statistics needed for substitution matrices. It is a
-- very modest attempt to replicate some of the Blast statistics.

module Biobase.SubstMatrix.Statistics where

import Data.Vector.Unboxed (Unbox)
import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Monadic as SM

import Data.PrimitiveArray as PA
import           Biobase.Primary.Letter

import Biobase.SubstMatrix.Types



-- | estimate Blast lambda.
--
-- TODO consider if different lambda 

estimateLambda
  ∷ (Unbox s, Num s)
  ⇒ AASubstMat t s a → Double
estimateLambda (AASubstMat mat) = go 1000 1 2 0 where
  go count lambda high low
    | count <= 0 = error "no convergence?!"
    | (high-low) <= 0.001 = lambda
    | otherwise           = undefined
    where
      -- get the rows and columns, needed to get the probs for each row/column right.
      (ZZ:..LtLetter r':..LtLetter c') = PA.upperBound mat
--      r = fromRational r' ∷ Double; c = fromRational c'
      -- sum of all scores
      s = unId . SM.foldl' (+) 0 $ SM.map eachElem $ PA.assocsS mat
      eachElem (Z:.i:.j, z) = z
