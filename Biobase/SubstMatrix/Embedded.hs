
module Biobase.SubstMatrix.Embedded where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.FileEmbed
import Control.Arrow (second)

import Statistics.Odds

import Biobase.SubstMatrix.Import
import Biobase.SubstMatrix.Types



embeddedPamBlosumFiles ∷ [(FilePath,ByteString)]
embeddedPamBlosumFiles = $(embedDir "sources/PamBlosum")

embeddedPamBlosum ∷ [(FilePath,AASubstMat t DiscLogOdds)]
embeddedPamBlosum = either error id . runExcept
                  . mapM (\(k,v) → fromByteString v >>= \mv → return (k,mv))
                  $ embeddedPamBlosumFiles

