{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Aeson.TH
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
    _blastoutput2 :: BlastOutput2
  }
  deriving (Show, Eq, Generic)

newtype BlastCmdJSON2 = BlastCmdJSON2
  {
    _blastcmdoutput2 :: [BlastOutput2]
  }
  deriving (Show, Eq, Generic)

--instance FromJSON BlastJSON2 where
--  parseJSON = genericParseJSON opts . jsonLower
--    where
--      opts = defaultOptions { fieldLabelModifier = map toLower }

newtype BlastOutput2 = BlastOutput2
  {
    _report :: BlastReport
  }
  deriving (Show, Eq, Generic)

data BlastReport = BlastReport
  { _program :: !T.Text,
    _version :: !T.Text,
    _reference :: !T.Text,
    _search_target :: !SearchTarget,
    _params :: !Params,
    _results :: !BlastJSONResult
  }
  deriving (Show, Eq, Generic)

newtype SearchTarget =  SearchTarget
  {
    _db :: T.Text
  }
  deriving (Show, Eq, Generic)

data Params = Params
  {
    _expect :: !Double,
    _sc_match :: !Int,
    _sc_mismatch :: !Int,
    _gap_open :: !Int,
    _gap_extend :: !Int,
    _filter :: !T.Text
  }
  deriving (Show, Eq, Generic)

data BlastJSONResult = BlastJSONResult
  {
    _search :: !Search
  }
  deriving (Show, Eq, Generic)

data Search = Search
  {
    _query_id :: !T.Text,
    _query_title :: !T.Text,
    _query_len :: !Int,
    _hits :: DS.Seq Hit,
    _stat :: !SearchStat
  }
  deriving (Show, Eq, Generic)

data Hit = Hit
  {
    _num :: !Int,
    _description :: ![HitDescription],
    _len :: !Int,
    _hsps :: ![Hsp]
  }
  deriving (Show, Eq, Generic)

data Hsp = Hsp
  {
    _hsp_num :: !Int, --actually just num in the output, but duplicate field exits in Hit
    _bit_score :: !Double,
    _score :: !Int,
    _evalue :: !Double,
    _identity :: !Int,
    _query_from :: !Int,
    _query_to :: !Int,
    _query_strand :: !T.Text,
    _hit_from :: !Int,
    _hit_to :: !Int,
    _hit_strand :: !T.Text,
    _align_len :: !Int,
    _gaps :: !Int,
    _qseq :: !T.Text,
    _hseq :: !T.Text,
    _midline :: !T.Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Hsp where
 parseJSON (Object v) =
    Hsp <$> v .: "num"
        <*> v .: "bit_score"
        <*> v .: "score"
        <*> v .: "evalue"
        <*> v .: "identity"
        <*> v .: "query_from"
        <*> v .: "query_to"
        <*> v .: "query_strand"
        <*> v .: "hit_from"
        <*> v .: "hit_to"
        <*> v .: "hit_strand"
        <*> v .: "align_len"
        <*> v .: "gaps"
        <*> v .: "qseq"
        <*> v .: "hseq"
        <*> v .: "midline"
 parseJSON _ = mzero


data HitDescription = HitDescription
  {
    _id :: !T.Text,
    _accession :: !T.Text,
    _title :: !T.Text,
    _taxid :: Maybe Int
    --_sciname :: !T.Text
  }
  deriving (Show, Eq, Generic)

data SearchStat = SearchStat {
    _db_num :: !Int,
    _db_len :: !Int,
    _hsp_len :: !Int,
    _eff_space :: !Int,
    _kappa :: !Double,
    _lambda :: !Double,
    _entropy :: !Double
  }
  deriving (Show, Eq, Generic)

data BlastTabularResult = BlastTabularResult
  { _blastProgram :: !BlastProgram,
    _blastQueryId :: !B.ByteString,
--    blastQueryName :: !B.ByteString,
    _blastDatabase :: !B.ByteString,
    _blastHitNumber :: !Int,
    _hitLines :: !(V.Vector BlastTabularHit)
  }
  deriving (Show, Eq)

data BlastTabularHit = BlastTabularHit
  { _queryId :: !B.ByteString,
    _subjectId ::  !B.ByteString,
    _seqIdentity :: !Double,
    _alignmentLength :: !Int,
    _misMatches :: !Int,
    _gapOpenScore :: !Int,
    _queryStart :: !Int,
    _queryEnd :: !Int,
    _hitSeqStart :: !Int,
    _hitSeqEnd :: !Int,
    _eValue :: !Double,
    _bitScore :: !Double,
    _subjectFrame :: !Int,
    _querySeq  :: !B.ByteString,
    _subjectSeq  :: !B.ByteString
  }
  deriving (Show, Eq)

data BlastProgram = BlastX | BlastP | BlastN
  deriving (Show, Eq)

makeLenses ''BlastCmdJSON2
--deriveJSON defaultOptions{fieldLabelModifier = (map toLower) . drop 1} ''BlastJSON2
instance FromJSON BlastCmdJSON2 where
  parseJSON (Object v) =
    BlastCmdJSON2 <$> (v .: "BlastOutput2")
  parseJSON _ = mzero
instance ToJSON BlastCmdJSON2 where
  toJSON (BlastCmdJSON2 _blastoutput2) =
        object [ "BlastOutput2"  Data.Aeson.Types..= _blastoutput2 ]

makeLenses ''BlastJSON2
--deriveJSON defaultOptions{fieldLabelModifier = (map toLower) . drop 1} ''BlastJSON2
instance FromJSON BlastJSON2 where
  parseJSON (Object v) = BlastJSON2 <$> (v .: "BlastOutput2")
  parseJSON _ = mzero

instance ToJSON BlastJSON2 where
  toJSON (BlastJSON2 _blastoutput2) = object [ "BlastOutput2"  Data.Aeson.Types..= _blastoutput2 ]
--deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BlastJSON2
makeLenses ''BlastOutput2
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BlastOutput2
makeLenses ''BlastReport
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BlastReport
makeLenses ''Params
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Params
makeLenses ''BlastJSONResult
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BlastJSONResult
makeLenses ''Search
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Search
makeLenses ''Hit
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Hit
makeLenses ''Hsp
--deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Hsp
makeLenses ''HitDescription
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''HitDescription
makeLenses ''SearchStat
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''SearchStat
makeLenses ''BlastTabularResult
makeLenses ''BlastTabularHit
makeLenses ''SearchTarget
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''SearchTarget
makeLenses ''BlastProgram
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BlastProgram
