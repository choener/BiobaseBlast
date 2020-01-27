
module Biobase.SubstMatrix.Embedded where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.FileEmbed
import Control.Arrow (second)
import GHC.Real(Ratio(..))

import Numeric.Discretized
import Statistics.Odds

import Biobase.SubstMatrix.Import
import Biobase.SubstMatrix.Types



embeddedPamBlosumFiles ∷ [(FilePath,ByteString)]
embeddedPamBlosumFiles = $(makeRelativeToProject "sources/PamBlosum" >>= embedDir)

embeddedPamBlosum ∷ [(FilePath,AASubstMat t (DiscLogOdds (1:%1)) a b)]
embeddedPamBlosum = either error id . runExcept
                  . mapM (\(k,v) → fromByteString v >>= \mv → return (k,mv))
                  $ embeddedPamBlosumFiles
{-# NoInline embeddedPamBlosum #-}

