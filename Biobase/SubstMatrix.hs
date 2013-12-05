
module Biobase.SubstMatrix where

import qualified Data.Array.IArray  as A
import qualified Data.Array.Unboxed as U
import qualified Data.Map as M
import Data.Char (toLower)

import Biobase.Primary
import Biobase.Codon



-- | Substitution table for one amino acid with another

type SubstMatrix = U.Array (Char,Char) Int

-- | Substitution from three DNA nucleotides to an amino acid.

type Nuc3SubstMatrix = U.Array (Nuc,Nuc,Nuc,Char) Int

type Nuc2SubstMatrix = U.Array (Nuc,Nuc,Char) Int

type Nuc1SubstMatrix = U.Array (Nuc,Char) Int



-- *

mkNuc3SubstMatrix :: SubstMatrix -> Nuc3SubstMatrix
mkNuc3SubstMatrix mat = A.accumArray (\_ z -> z) (-10) ((nN,nN,nN,'*'),(nT,nT,nT,'Z'))
  [ ( (a,b,c,k), case l of Just l' -> mat A.! (l',k) ; Nothing -> -10 )
  | a<-acgt, b<-acgt, c<-acgt
  , k<-['*' .. 'Z']
  , let abc = map (toLower . fromNuc) [a,b,c]
  , let l = M.lookup abc codonTable
  ]

mkNuc2SubstMatrix :: (Int -> Int -> Int) -> (Int -> Int) -> SubstMatrix -> Nuc2SubstMatrix
mkNuc2SubstMatrix f g mat = A.accumArray f (-10) ((nN,nN,'*'),(nT,nT,'Z'))
  [ ( (x,y,k), case l of Just l' -> mat A.! (l',k) ; Nothing -> -10 )
  | a<-acgt, b<-acgt, c<-acgt
  , k<-['*' .. 'Z']
  , (x,y) <- [ (a,b), (a,c), (b,c) ]
  , let abc = map (toLower . fromNuc) [a,b,c]
  , let l = M.lookup abc codonTable
  ]

mkNuc1SubstMatrix :: (Int -> Int -> Int) -> (Int -> Int) -> SubstMatrix -> Nuc1SubstMatrix
mkNuc1SubstMatrix f g mat = A.accumArray f (-10) ((nN,'*'),(nT,'Z'))
  [ ( (x,k), case l of Just l' -> mat A.! (l',k) ; Nothing -> -10 )
  | a<-acgt, b<-acgt, c<-acgt
  , k<-['*' .. 'Z']
  , x <- [a,b,c]
  , let abc = map (toLower . fromNuc) [a,b,c]
  , let l = M.lookup abc codonTable
  ]

