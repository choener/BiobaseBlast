
module Biobase.SubstMatrix where

import           Control.DeepSeq (NFData(..))
import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.List (maximumBy)
import           Data.Serialize (Serialize)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

import           Biobase.GeneticCodes.Translation
import           Biobase.GeneticCodes.Types
import           Biobase.Primary.AA (AA,aaRange)
import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.DNA (DNA)
import           Biobase.Primary.Trans
import           Data.PrimitiveArray
import qualified Biobase.Primary.AA as AA
import qualified Biobase.Primary.Nuc.DNA as DNA
import           Statistics.Odds



-- | Denotes that we are dealing with a similarity score. Higher is more
-- similar.

data Similarity

-- | Denotes that we are dealing with a distance score. Lower is more
-- similar.

data Distance

-- An amino-acid substitution matrix. Tagged with the type of scoring used.

newtype AASubstMat t s = AASubstMat { _aaSubstMat :: Unboxed (Z:.Letter AA:.Letter AA) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''AASubstMat

--instance Binary    (AASubstMat t)
--instance Serialize (AASubstMat t)
--instance FromJSON  (AASubstMat t)
--instance (ToJSON s, VU.Unbox s, Generic s) ⇒ ToJSON    (AASubstMat t s)

--instance NFData (AASubstMat t s)

-- | @PAM@ matrices are similarity matrices.

type SubstPAM = AASubstMat Similarity DiscLogOdds

-- | @BLOSUM@ matrices are distance matrices.

type SubstBLOSUM = AASubstMat Distance DiscLogOdds

-- | Substitution matrix from amino acids to nucleotide triplets.

newtype ANuc3SubstMat t s = ANuc3SubstMat { _anuc3SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA:.Letter DNA:.Letter DNA) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc3SubstMat

--instance Binary    (ANuc3SubstMat t)
--instance Serialize (ANuc3SubstMat t)
--instance FromJSON  (ANuc3SubstMat t)
--instance ToJSON    (ANuc3SubstMat t)

--instance NFData (ANuc3SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 2-tuples. The third nucleotide letter is missing.

newtype ANuc2SubstMat t s = ANuc2SubstMat { _anuc2SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA:.Letter DNA) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc2SubstMat

--instance Binary    (ANuc2SubstMat t)
--instance Serialize (ANuc2SubstMat t)
--instance FromJSON  (ANuc2SubstMat t)
--instance ToJSON    (ANuc2SubstMat t)

--instance NFData (ANuc2SubstMat t)

-- | Substitution matrix from amino acids to degenerate nucleotide
-- 1-tuples. Two out of three nucleotides in a triplet are missing.

newtype ANuc1SubstMat t s = ANuc1SubstMat { _anuc1SubstMat :: Unboxed (Z:.Letter AA:.Letter DNA) s }
  deriving (Generic,Eq,Read,Show)
makeLenses ''ANuc1SubstMat

--instance Binary    (ANuc1SubstMat t)
--instance Serialize (ANuc1SubstMat t)
--instance FromJSON  (ANuc1SubstMat t)
--instance ToJSON    (ANuc1SubstMat t)

--instance NFData (ANuc1SubstMat t)

-- | The usual substitution matrix, but here with a codon and an amino acid
-- to be compared.
--
-- TODO Definitely use the correct upper bound constants here!

mkANuc3SubstMat
  ∷ TranslationTable (Letter DNA) (Letter AA)
  → AASubstMat t DiscLogOdds
  → ANuc3SubstMat t (Letter AA, DiscLogOdds)
mkANuc3SubstMat tbl (AASubstMat m)
  = ANuc3SubstMat
  $ fromAssocs (ZZ:..LtLetter AA.Undef:..LtLetter DNA.N:..LtLetter DNA.N:..LtLetter DNA.N) (AA.Undef, DiscLogOdds $ -999)
    [ ( (Z:.a:.u:.v:.w)
      , (t, m!(Z:.a:.t))
      )
    | a <- aaRange
    , u <- [DNA.A .. DNA.N], v <- [DNA.A .. DNA.N], w <- [DNA.A .. DNA.N]
    , let b = BaseTriplet u v w, let t = translate tbl b
    ]

{-
-- | Create a 2-tuple to amino acid substitution matrix. Here, @f@ combines
-- all to entries that have the same 2-tuple index.

mkANuc2SubstMat
  ∷ ((Z:.Letter AA:.Letter DNA:.Letter DNA) → (Letter AA, DiscLogOdds) → (Letter AA, DiscLogOdds) → Ordering)
  → AASubstMat t DiscLogOdds
  → ANuc2SubstMat t (Letter AA, DiscLogOdds)
mkANuc2SubstMat f (AASubstMat m)
  = ANuc2SubstMat
  $ fromAssocs (ZZ:..LtLetter (length aaRange):..LtLetter 5:..LtLetter 5) (AA.Undef, DiscLogOdds $ -999)
  . M.assocs
  . M.mapWithKey (\k → maximumBy (f k))
  . M.fromListWith (++)
  $ [ ((Z:.a:.x:.y), [maybe (AA.Undef, DiscLogOdds $ -999) (\k -> (k, m!(Z:.a:.k))) $ M.lookup uvw dnaAAmap])
    | a <- aaRange
    , u <- [DNA.A .. DNA.N], v <- [DNA.A .. DNA.N], w <- [DNA.A .. DNA.N]
    , (x,y) <- [ (u,v), (u,w), (v,w) ]
    , let uvw = VU.fromList [u,v,w]
    ]

-- | The most degenerate case, where just a single nucleotide remains in
-- the amino-acid / nucleotide substitution. Again, @f@ combines different
-- entries.

mkANuc1SubstMat
  ∷ ((Z:.Letter AA:.Letter DNA) → (Letter AA, DiscLogOdds) → (Letter AA, DiscLogOdds) → Ordering)
  → AASubstMat t DiscLogOdds
  → ANuc1SubstMat t (Letter AA, DiscLogOdds)
mkANuc1SubstMat f (AASubstMat m)
  = ANuc1SubstMat
  $ fromAssocs (ZZ:..LtLetter (length aaRange):..LtLetter 5) (AA.Undef, DiscLogOdds $ -999)
  . M.assocs
  . M.mapWithKey (\k → maximumBy (f k))
  . M.fromListWith (++)
  $ [ ((Z:.a:.x), [maybe (AA.Undef, DiscLogOdds $ -999) (\k -> (k, m!(Z:.a:.k))) $ M.lookup uvw dnaAAmap])
    | a <- aaRange
    , u <- [DNA.A .. DNA.N], v <- [DNA.A .. DNA.N], w <- [DNA.A .. DNA.N]
    , x <- [u,v,w]
    , let uvw = VU.fromList [u,v,w]
    ]
-}

