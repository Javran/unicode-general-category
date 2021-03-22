module Data.Char.GeneralCategory.V13_0_0Spec where

import Data.Char (GeneralCategory (..))
import qualified Data.Char as BaseChar
import Data.Char.GeneralCategory.Database
import qualified Data.Char.GeneralCategory.V13_0_0 as U13
import Data.Either
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck

{-

  Notes are based on the results of following setup:

  - The Glorious Glasgow Haskell Compilation System, version 8.8.4
  - Unicode 13.0.0

  5 known inconsistent characters:

  + ('\5741',(OtherPunctuation,OtherSymbol))
    https://unicode.org/reports/tr44/
    The Terminal_Punctuation property of U+166D CANADIAN SYLLABICS CHI SIGN was changed to No

  + ('\43453',(SpacingCombiningMark,NonSpacingMark))
    https://unicode.org/reports/tr44/
    The classification of the dependent form of the Javanese vocalic r,
    U+A9BD JAVANESE CONSONANT SIGN KERET, was corrected to a below-base mark

  + ('\72146',(NonSpacingMark,SpacingCombiningMark))
    https://www.unicode.org/L2/L2019/19047-script-adhoc-recs.pdf

  + ('\72162',(OtherLetter,OtherPunctuation))
    not sure about this one, it's already Po in Unicode 12.0.0 and Unicode 12.1.0.

  + ('\123215',(OtherLetter,OtherSymbol))
    https://www.unicode.org/L2/L2019/19008.htm
    "Update the general category of U+1E14F NYIAKENG PUACHUE HMONG CIRCLED CA
    from gc="Lo" to "So", for Unicode version 12.0."
 -}
knownInconsistencies :: S.Set Char
knownInconsistencies = S.fromList "\5741\43453\72146\72162\123215"

spec :: Spec
spec = do
  describe "genCatDb" $
    specify "checkDatabase" $
      Blind U13.genCatDb `shouldSatisfy` isRight . checkDatabase' . getBlind
  describe "generalCategory" $
    specify "consistency check against base" $ do
      {-
        check against all chars.
        this also serves as verifying that query is implemented correctly.
       -}
      let allChars :: [Char]
          allChars = [minBound .. maxBound]
          inconsistents
            :: [ ( Char
                 , ( GeneralCategory -- general category from base
                   , GeneralCategory -- general category from UnicodeData.txt
                   )
                 )
               ]
          inconsistents = concatMap getInconsistent allChars
            where
              getInconsistent ch =
                [(ch, (libGc, u13)) | libGc /= NotAssigned, u13 /= libGc]
                where
                  libGc = BaseChar.generalCategory ch
                  u13 = U13.generalCategory ch
          unexpected =
            S.difference
              (S.fromList (fmap fst inconsistents))
              knownInconsistencies
      unexpected `shouldBe` S.empty
