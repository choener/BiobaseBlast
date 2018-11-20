{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Encoding of tabular NCBI BLAST+ output

module Biobase.BLAST.Types where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import qualified Data.Text as T
import System.Directory
import Data.Char
import Control.Monad
import Text.Printf
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as DS
import Control.Lens

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

newtype BlastJSON2 = BlastJSON2
  {
    blastoutput2 :: BlastOutput2
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON BlastJSON2 where
  parseJSON = genericParseJSON opts . jsonLower
    where
      opts = defaultOptions { fieldLabelModifier = map toLower}

newtype BlastOutput2 = BlastOutput2
  {
    report :: BlastReport
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BlastReport = BlastReport
  { program :: !T.Text,
    version :: !T.Text,
    reference :: !T.Text,
    search_target :: !SearchTarget,
    params :: !Params,
    results :: !BlastJSONResult
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SearchTarget =  SearchTarget
  {
    db :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Params = Params
  {
    expect :: !Double,
    sc_match :: !Int,
    sc_mismatch :: !Int,
    gap_open :: !Int,
    gap_extend :: !Int,
    filter :: !T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BlastJSONResult = BlastJSONResult
  {
    search :: !Search
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Search = Search
  {
    query_id :: !T.Text,
    query_title :: !T.Text,
    query_len :: !Int,
    hits :: DS.Seq Hit,
    stat :: !SearchStat
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Hit = Hit
  {
    num :: !Int,
    description :: ![HitDescription],
    len :: !Int,
    hsps :: ![Hsp]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Hsp = Hsp
  {
    num :: !Int,
    bit_score :: !Double,
    score :: !Int,
    evalue :: !Double,
    identity :: !Int,
    query_from :: !Int,
    query_to :: !Int,
    query_strand :: !T.Text,
    hit_from :: !Int,
    hit_to :: !Int,
    hit_strand :: !T.Text,
    align_len :: !Int,
    gaps :: !Int,
    qseq :: !T.Text,
    hseq :: !T.Text,
    midline :: !T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HitDescription = HitDescription
  {
    id :: !T.Text,
    accession :: !T.Text,
    title :: !T.Text,
    taxid :: !Int,
    sciname :: !T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SearchStat = SearchStat {
    db_num :: !Int,
    db_len :: !Int,
    hsp_len :: !Int,
    eff_space :: !Int,
    kappa :: !Double,
    lambda :: !Double,
    entropy :: !Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BlastTabularResult = BlastTabularResult
  { blastProgram :: !BlastProgram,
    blastQueryId :: !B.ByteString,
--    blastQueryName :: !B.ByteString,
    blastDatabase :: !B.ByteString,
    blastHitNumber :: !Int,
    hitLines :: !(V.Vector BlastTabularHit)
  }
  deriving (Show, Eq)

data BlastTabularHit = BlastTabularHit
  { queryId :: !B.ByteString,
    subjectId ::  !B.ByteString,
    seqIdentity :: !Double,
    alignmentLength :: !Int,
    misMatches :: !Int,
    gapOpenScore :: !Int,
    queryStart :: !Int,
    queryEnd :: !Int,
    hitSeqStart :: !Int,
    hitSeqEnd :: !Int,
    eValue :: !Double,
    bitScore :: !Double,
    subjectFrame :: !Int,
    querySeq  :: !B.ByteString,
    subjectSeq  :: !B.ByteString
  }
  deriving (Show, Eq)

data BlastProgram = BlastX | BlastP | BlastN
  deriving (Show, Eq)

makeLenses ''BlastJSON2
makeLenses ''BlastOutput2
makeLenses ''BlastReport
makeLenses ''Params
makeLenses ''BlastJSONResult
makeLenses ''Search
makeLenses ''Hit
makeLenses ''Hsp
makeLenses ''HitDescription
makeLenses ''SearchStat
makeLenses ''BlastTabularResult
makeLenses ''BlastTabularHit
makeLenses ''SearchTarget
makeLenses ''BlastProgram

