
module Biobase.SubstMatrix.Import where

import qualified Data.Array.IArray as A
import Control.Applicative

import Biobase.SubstMatrix



fromFile :: FilePath -> IO SubstMatrix
fromFile fname = do
  (x:xs) <- dropWhile (("#"==).take 1) . lines <$> readFile fname
  let cs = map head . words $ x -- should give us the characters encoding an amino acid
  let ss = map (map read . drop 1 . words) $ xs
  return $ A.accumArray (\_ z -> z) (-10) (('*','*'),('Z','Z')) [ ((k1,k2),z) | (k1,s) <- zip cs ss, (k2,z) <- zip cs s ]

