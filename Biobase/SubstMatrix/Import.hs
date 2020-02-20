
-- | Import PAM/BLOSUM substituion matrices.

module Biobase.SubstMatrix.Import where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           System.Directory (doesFileExist)

import           Biobase.Primary.AA (charAA)
import           Biobase.Primary.Letter (getLetter,LimitType(..))
import           Data.PrimitiveArray hiding (map)
import           Numeric.Discretized
import qualified Biobase.Primary.AA as AA
import           Statistics.Odds

import           Biobase.SubstMatrix.Types
import           Biobase.SubstMatrix.Statistics



-- | Import substituion matrix from a bytestring.
--
-- TODO the parser is fragile, since it uses @read@. This should be fixed.

fromByteString
  :: ( MonadError String m
     , Real (Discretized k)
     )
  => ByteString
  -> m (AASubstMat t (DiscLogOdds k) a b)
fromByteString bstring = do
  let (b:bs) = dropWhile (("#"==).take 1) . lines $ unpack bstring
      cs = map head . words $ b -- should give us the characters encoding an amino acid
      ss = map (map (DiscLogOdds . Discretized) . map read . drop 1 . words) $ bs
      xs = [ ((Z:.charAA k1:.charAA k2),z)
           | (k1,s) <- zip cs ss
           , (k2,z) <- zip cs s
           ]
      as = fromAssocs (ZZ:..LtLetter AA.Z:..LtLetter AA.Z) (DiscLogOdds . Discretized $ -999) xs
      mat = AASubstMat as (estimateLambda mat)
  return mat

-- | Import substitution matrix from file.

fromFile
  :: ( MonadIO m, MonadError String m
     , Real (Discretized k)
     )
  => FilePath
  -> m (AASubstMat t (DiscLogOdds k) a b)
fromFile fname = liftIO (BS.readFile fname) >>= fromByteString

