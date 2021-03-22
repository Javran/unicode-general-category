{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | This module offers functions related to 'Data.Char.GeneralCategory'
--    but queries Unicode 13.0.0 database.
module Data.Char.GeneralCategory.V13_0_0
  ( generalCategory
  , isLetter
  , isMark
  , isNumber
  , isPunctuation
  , isSymbol
  , isSeparator
  , genCatDb
  )
where

import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Char.GeneralCategory.Database
import qualified Data.Char.GeneralCategory.Predicates as GCP
import Data.FileEmbed

genCatDb :: GenCatDatabase
genCatDb = decode $ BSL.fromStrict $(embedFile "embed/v13.0.0.raw")

GCP.Predicates {..} = GCP.mkPredicates (query genCatDb)
