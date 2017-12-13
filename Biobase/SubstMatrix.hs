
module Biobase.SubstMatrix where

import           Control.DeepSeq (NFData(..))
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Serialize (Serialize)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.AA (AA,aaRange)
import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.DNA (DNA)
import           Biobase.Primary.Trans (dnaAAmap)
import           Biobase.Types.Odds
import           Data.PrimitiveArray
import qualified Biobase.Primary.AA as AA
import qualified Biobase.Primary.Nuc.DNA as D



-- | Denotes that we are dealing with a similarity score. Higher is more
-- similar.

data Similarity

-- | Denotes that we are dealing with a distance score. Lower is more
-- similar.

data Distance

-- An amino-acid substitution matrix. Tagged with the type of scoring used.

newtype AASubstMat t = AASubstMat { aaSubstMat :: Unboxed (Z:.Letter AA:.Letter AA) DiscretizedLogOdds }
  deriving (Generic,Eq,Read,Show)

--instance Binary    (AASubstMat t)
--instance Serialize (AASubstMat t)
--instance FromJSON  (AASubstMat t)
--instance ToJSON    (AASubstMat t)

--instance NFData (AASubstMat t)

-- | @PAM@ matrices are similarity matrices.

type SubstPAM = AASubstMat Similarity

-- | @BLOSUM@ matrices are distance matrices.

type SubstBLOSUM = AASubstMat Distance

-- | Substitution matrix from amino acids to nucleotide triplets.

newtype ANuc3SubstMat t = ANuc3SubstMat { anuc3SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA:.Letter DNA:.Letter DNA) DiscretizedLogOdds }
  deriving (Generic,Eq,Read,Show)

--instance Binary    (ANuc3SubstMat t)
--instance Serialize (ANuc3SubstMat t)
--instance FromJSON  (ANuc3SubstMat t)
--instance ToJSON    (ANuc3SubstMat t)

--instance NFData (ANuc3SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 2-tuples. The third nucleotide letter is missing.

newtype ANuc2SubstMat t = ANuc2SubstMat { anuc2SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA:.Letter DNA) DiscretizedLogOdds }
  deriving (Generic,Eq,Read,Show)

--instance Binary    (ANuc2SubstMat t)
--instance Serialize (ANuc2SubstMat t)
--instance FromJSON  (ANuc2SubstMat t)
--instance ToJSON    (ANuc2SubstMat t)

--instance NFData (ANuc2SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 1-tuples. Two out of three nucleotides in a triplet are missing.

newtype ANuc1SubstMat t = ANuc1SubstMat { anuc1SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA) DiscretizedLogOdds }
  deriving (Generic,Eq,Read,Show)

--instance Binary    (ANuc1SubstMat t)
--instance Serialize (ANuc1SubstMat t)
--instance FromJSON  (ANuc1SubstMat t)
--instance ToJSON    (ANuc1SubstMat t)

--instance NFData (ANuc1SubstMat t)

-- | The usual substitution matrix, but here with a codon and an amino acid
-- to be compared.
--
-- TODO Definitely use the correct upper bound constants here!

mkANuc3SubstMat :: AASubstMat t -> ANuc3SubstMat t
mkANuc3SubstMat (AASubstMat m) = ANuc3SubstMat $ fromAssocs (ZZ:..LtLetter 20:..LtLetter 5:..LtLetter 5:..LtLetter 5) (DLO $ -999)
  [ ( (Z:.a:.u:.v:.w) , maybe (DLO $ -999) (\b -> m!(Z:.a:.b)) $ M.lookup uvw dnaAAmap)
  | a <- aaRange
  , u <- [D.A .. D.N], v <- [D.A .. D.N], w <- [D.A .. D.N]
  , let uvw = VU.fromList [u,v,w]
  ]

-- | Create a 2-tuple to amino acid substitution matrix. Here, @f@ combines
-- all to entries that have the same 2-tuple index.

mkANuc2SubstMat :: (DiscretizedLogOdds -> DiscretizedLogOdds -> DiscretizedLogOdds) -> AASubstMat t -> ANuc2SubstMat t
mkANuc2SubstMat f (AASubstMat m) = ANuc2SubstMat $ fromAssocs (ZZ:..LtLetter 20:..LtLetter 5:..LtLetter 5) (DLO $ -999)
  . M.assocs
  . M.fromListWith f
  $ [ ((Z:.a:.x:.y), maybe (DLO $ -999) (\k -> m!(Z:.a:.k)) $ M.lookup uvw dnaAAmap)
    | a <- aaRange
    , u <- [D.A .. D.N], v <- [D.A .. D.N], w <- [D.A .. D.N]
    , (x,y) <- [ (u,v), (u,w), (v,w) ]
    , let uvw = VU.fromList [u,v,w]
    ]

-- | The most degenerate case, where just a single nucleotide remains in
-- the amino-acid / nucleotide substitution. Again, @f@ combines different
-- entries.

mkANuc1SubstMat :: (DiscretizedLogOdds -> DiscretizedLogOdds -> DiscretizedLogOdds) -> AASubstMat t -> ANuc1SubstMat t
mkANuc1SubstMat f (AASubstMat m) = ANuc1SubstMat $ fromAssocs (ZZ:..LtLetter 20:..LtLetter 5) (DLO $ -999)
  . M.assocs
  . M.fromListWith f
  $ [ ((Z:.a:.x), maybe (DLO $ -999) (\k -> m!(Z:.a:.k)) $ M.lookup uvw dnaAAmap)
    | a <- aaRange
    , u <- [D.A .. D.N], v <- [D.A .. D.N], w <- [D.A .. D.N]
    , x <- [u,v,w]
    , let uvw = VU.fromList [u,v,w]
    ]

