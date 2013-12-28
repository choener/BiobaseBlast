
module Biobase.SubstMatrix.Import where

import           Control.Applicative
import qualified Data.Map as M
import           Data.Char (toLower)
import           Data.Array.Repa.Index
import           Data.List (minimumBy)

import qualified Data.PrimitiveArray as PA
import qualified Data.PrimitiveArray.Zero as PA
import           Biobase.Primary
import           Biobase.AAseq

import Biobase.SubstMatrix



fromFile :: FilePath -> IO SubstMatrix
fromFile fname = do
  (x:xs) <- dropWhile (("#"==).take 1) . lines <$> readFile fname
  let cs = map (head) . words $ x -- should give us the characters encoding an amino acid
  let ss = map (map read . drop 1 . words) $ xs
  return $ PA.fromAssocs (Z:.aStop:.aStop) (Z:.aY:.aY) (-999) $
    [ ((Z:.toAA k1:.toAA k2),z)
    | (k1,s) <- zip cs ss
    , k1/='*'
    , (k2,z) <- zip cs s
    , k2/='*'
    ]
  --return $ A.accumArray (\_ z -> z) (-10) (('*','*'),('Z','Z')) [ ((k1,k2),z) | (k1,s) <- zip cs ss, (k2,z) <- zip cs s ]

