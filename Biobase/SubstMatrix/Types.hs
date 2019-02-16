
module Biobase.SubstMatrix.Types where

import Control.Lens
import GHC.Generics (Generic)

import Algebra.Structure.Semiring
import Biobase.Types.BioSequence (AA,DNA)
import Biobase.Primary.AA (aaRange)
import Biobase.Primary.Letter
import Biobase.Primary.Nuc.DNA ()
import Data.PrimitiveArray
import Statistics.Odds
import Numeric.Discretized



-- | Denotes that we are dealing with a similarity score. Higher is more
-- similar.

data Similarity

-- | Denotes that we are dealing with a distance score. Lower is more
-- similar.

data Distance

-- An amino-acid substitution matrix. Tagged with the type of scoring used.

newtype AASubstMat t s a = AASubstMat { _aaSubstMat :: Unboxed (Z:.Letter AA a:.Letter AA a) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''AASubstMat

--instance Binary    (AASubstMat t)
--instance Serialize (AASubstMat t)
--instance FromJSON  (AASubstMat t)
--instance (ToJSON s, VU.Unbox s, Generic s) ⇒ ToJSON    (AASubstMat t s)

--instance NFData (AASubstMat t s)

-- | @PAM@ matrices are similarity matrices.

type SubstPAM = AASubstMat Similarity (DiscLogOdds Unknown)

-- | @BLOSUM@ matrices are distance matrices.

type SubstBLOSUM = AASubstMat Distance (DiscLogOdds Unknown)

-- | Substitution matrix from amino acids to nucleotide triplets.

newtype ANuc3SubstMat t s a n = ANuc3SubstMat { _anuc3SubstMat :: Unboxed (Z:.Letter AA a:.Letter DNA n:.Letter DNA n:.Letter DNA n) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc3SubstMat

--instance Binary    (ANuc3SubstMat t)
--instance Serialize (ANuc3SubstMat t)
--instance FromJSON  (ANuc3SubstMat t)
--instance ToJSON    (ANuc3SubstMat t)

--instance NFData (ANuc3SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 2-tuples. The third nucleotide letter is missing.

newtype ANuc2SubstMat t s a n = ANuc2SubstMat { _anuc2SubstMat :: Unboxed (Z:.Letter AA a:.Letter DNA n:.Letter DNA n) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc2SubstMat

--instance Binary    (ANuc2SubstMat t)
--instance Serialize (ANuc2SubstMat t)
--instance FromJSON  (ANuc2SubstMat t)
--instance ToJSON    (ANuc2SubstMat t)

--instance NFData (ANuc2SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 1-tuples. Two out of three nucleotides in a triplet are missing.

newtype ANuc1SubstMat t s a n = ANuc1SubstMat { _anuc1SubstMat :: Unboxed (Z:.Letter AA a:.Letter DNA n) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc1SubstMat

--instance Binary    (ANuc1SubstMat t)
--instance Serialize (ANuc1SubstMat t)
--instance FromJSON  (ANuc1SubstMat t)
--instance ToJSON    (ANuc1SubstMat t)

--instance NFData (ANuc1SubstMat t)



data GapCost t = GapCost
  { gcInit ∷ !t
  , gcCont ∷ !t
  }
