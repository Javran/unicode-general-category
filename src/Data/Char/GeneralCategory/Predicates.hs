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

data Predicates i = Predicates
  { generalCategory :: i -> GeneralCategory
  , isLetter :: i -> Bool
  , isMark :: i -> Bool
  , isNumber :: i -> Bool
  , isPunctuation :: i -> Bool
  , isSymbol :: i -> Bool
  , isSeparator :: i -> Bool
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

mkPredicates :: (Char -> GeneralCategory) -> Predicates Char
mkPredicates = (>$< predicates)
