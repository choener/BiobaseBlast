
-- | Encoding of tabular NCBI BLAST+ output

module Biobase.BLAST.Types where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import System.Directory
import Data.Char
import Control.Monad
import Debug.Trace
import Text.Printf

data BlastTabularResult = BlastTabularResult
  { blastProgram :: !BlastProgram,
    blastQueryId :: !B.ByteString,
--    blastQueryName :: !B.ByteString,
    blastDatabase :: !B.ByteString,
    blastHitNumber :: !Int,
    hitLines :: !(V.Vector BlastTabularHit)
  }
  deriving (Show, Eq)

data BlastProgram = BlastX | BlastP | BlastN
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