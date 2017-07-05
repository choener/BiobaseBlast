{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | NCBI BLAST
--

module Biobase.BLAST
  ( module Biobase.BLAST.Types
  , module Biobase.BLAST.Import
  ) where

import Biobase.BLAST.Import (blastFromFile)
import Biobase.BLAST.Types

