
-- | Import PAM/BLOSUM substituion matrices.

module Biobase.SubstMatrix.Import where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Char (toLower)
import qualified Data.Map as M

import           Biobase.Primary.AA (charAA)
import           Biobase.Primary.Letter (getLetter,LimitType(..))
import           Data.PrimitiveArray hiding (map)
import qualified Biobase.Primary.AA as AA
import           Statistics.Odds

import           Biobase.SubstMatrix



-- | Import substitution matrix from file.

fromFile ∷ (MonadIO m, MonadError String m) ⇒ FilePath → m (AASubstMat t DiscLogOdds)
fromFile fname = do
  (x:xs) ← dropWhile (("#"==).take 1) . lines <$> liftIO (readFile fname)
  let cs = map head . words $ x -- should give us the characters encoding an amino acid
  let ss = map (map DiscLogOdds . map read . drop 1 . words) $ xs
  let xs = [ ((Z:.charAA k1:.charAA k2),z)
           | (k1,s) <- zip cs ss
           , (k2,z) <- zip cs s
           ]
  return . AASubstMat $ fromAssocs (ZZ:..LtLetter AA.Z:..LtLetter AA.Z) (DiscLogOdds $ -999) xs

