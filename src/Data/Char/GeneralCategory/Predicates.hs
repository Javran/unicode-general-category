module Data.Char.GeneralCategory.Predicates
  ( Predicates (..)
  , predicates
  , mkPredicates
  )
where

import Data.Char (GeneralCategory (..))
import Data.Functor.Contravariant

gcIsLetter :: GeneralCategory -> Bool
gcIsLetter c = case c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  _ -> False

gcIsMark :: GeneralCategory -> Bool
gcIsMark c = case c of
  NonSpacingMark -> True
  SpacingCombiningMark -> True
  EnclosingMark -> True
  _ -> False

gcIsNumber :: GeneralCategory -> Bool
gcIsNumber c = case c of
  DecimalNumber -> True
  LetterNumber -> True
  OtherNumber -> True
  _ -> False

gcIsPunctuation :: GeneralCategory -> Bool
gcIsPunctuation c = case c of
  ConnectorPunctuation -> True
  DashPunctuation -> True
  OpenPunctuation -> True
  ClosePunctuation -> True
  InitialQuote -> True
  FinalQuote -> True
  OtherPunctuation -> True
  _ -> False

gcIsSymbol :: GeneralCategory -> Bool
gcIsSymbol c = case c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  _ -> False

gcIsSeparator :: GeneralCategory -> Bool
gcIsSeparator c = case c of
  Space -> True
  LineSeparator -> True
  ParagraphSeparator -> True
  _ -> False

-- | A set of predicate functions related to `GeneralCategory` queries.
--   You can either destruct at top level or locally to get access to each of them.
data Predicates i = Predicates
  { -- | Counterpart of 'Data.Char.generalCategory'
    generalCategory :: i -> GeneralCategory
  , -- | Counterpart of 'Data.Char.isLetter'
    isLetter :: i -> Bool
  , -- | Counterpart of 'Data.Char.isMark'
    isMark :: i -> Bool
  , -- | Counterpart of 'Data.Char.isNumber'
    isNumber :: i -> Bool
  , -- | Counterpart of 'Data.Char.isPunctuation'
    isPunctuation :: i -> Bool
  , -- | Counterpart of 'Data.Char.isSymbol'
    isSymbol :: i -> Bool
  , -- | Counterpart of 'Data.Char.isSeparator'
    isSeparator :: i -> Bool
  }

instance Contravariant Predicates where
  contramap f (Predicates g l m n p sy se) =
    Predicates
      (g . f)
      (l . f)
      (m . f)
      (n . f)
      (p . f)
      (sy . f)
      (se . f)

-- | A set of functions similiar to their counterparts found in "Data.Char"
--   but takes 'GeneralCategory' as argument.
predicates :: Predicates GeneralCategory
predicates =
  Predicates
    { isLetter = gcIsLetter
    , isMark = gcIsMark
    , isNumber = gcIsNumber
    , isPunctuation = gcIsPunctuation
    , isSymbol = gcIsSymbol
    , isSeparator = gcIsSeparator
    , generalCategory = id
    }

-- | Takes a function that is equivalent to 'Data.Char.generalCategory'
--   and returns the set of functions equivalent to those found in "Data.Char",
--   but with the argument function serving instead.
mkPredicates :: (Char -> GeneralCategory) -> Predicates Char
mkPredicates = (>$< predicates)
