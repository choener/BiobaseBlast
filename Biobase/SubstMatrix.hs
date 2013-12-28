{-# LANGUAGE TypeOperators #-}

module Biobase.SubstMatrix where

import qualified Data.Map as M
import           Data.Char (toLower)
import           Data.Array.Repa.Index
import           Data.List (minimumBy)

import qualified Data.PrimitiveArray as PA
import qualified Data.PrimitiveArray.Zero as PA
import           Biobase.Primary
import           Biobase.AAseq



-- | Substitution table for one amino acid with another

type SubstMatrix = PA.Unboxed (Z:.AA:.AA) Int -- U.Array (Char,Char) Int

-- | Substitution from three DNA nucleotides to an amino acid.

type Nuc3SubstMatrix = PA.Unboxed (Z:.Nuc:.Nuc:.Nuc:.AA) Int -- U.Array (Nuc,Nuc,Nuc,Char) Int

type Nuc2SubstMatrix = PA.Unboxed (Z:.Nuc:.Nuc:.AA) Int -- U.Array (Nuc,Nuc,Char) Int

type Nuc1SubstMatrix = PA.Unboxed (Z:.Nuc:.AA) Int -- U.Array (Nuc,Char) Int



-- * Create nucleotide-based substitution matrices.

-- | The usual matrix, where a codon and an amino acid are compared.

mkNuc3SubstMatrix :: SubstMatrix -> Nuc3SubstMatrix
mkNuc3SubstMatrix mat = PA.fromAssocs (Z:.nN:.nN:.nN:.aStop) (Z:.nT:.nT:.nT:.aY) (-999)
  [ ( (Z:.a:.b:.c:.k), case l of Just l' -> mat PA.! (Z:.l':.k) ; Nothing -> -999 )
  | a<-acgt, b<-acgt, c<-acgt
  , k<-aaRange
  , let l = M.lookup [a,b,c] nucCodonTable
  ]

-- | Special substitution matrix, where two amino acids are compared to an
-- amino acid.

mkNuc2SubstMatrix :: (Int -> Int -> Int) -> (Int -> Int) -> SubstMatrix -> Nuc2SubstMatrix
mkNuc2SubstMatrix f g mat = PA.fromAssocs (Z:.nN:.nN:.aStop) (Z:.nT:.nT:.aY) (-999) -- ((nN,nN,'*'),(nT,nT,'Z'))
  . M.assocs
  . M.fromListWith f
  $ [ ( (Z:.x:.y:.k), case l of Just l' -> mat PA.! (Z:.l':.k) ; Nothing -> -999 )
    | a<-acgt, b<-acgt, c<-acgt
    , k<-aaRange
    , (x,y) <- [ (a,b), (a,c), (b,c) ]
    , let l = M.lookup [a,b,c] nucCodonTable
    ]

mkNuc1SubstMatrix :: (Int -> Int -> Int) -> (Int -> Int) -> SubstMatrix -> Nuc1SubstMatrix
mkNuc1SubstMatrix f g mat = PA.fromAssocs (Z:.nN:.aStop) (Z:.nT:.aY) (-999) -- A.accumArray f (-10) ((nN,'*'),(nT,'Z'))
  . M.assocs
  . M.fromListWith f
  $ [ ( (Z:.x:.k), case l of Just l' -> mat PA.! (Z:.l':.k) ; Nothing -> -999 )
    | a<-acgt, b<-acgt, c<-acgt
    , k<-aaRange
    , x <- [a,b,c]
    , let l = M.lookup [a,b,c] nucCodonTable
    ]

