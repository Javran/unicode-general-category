{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Char.GeneralCategory.Database
  ( genCatLitTable
  , mkDatabaseFromUnicodeData
  , GenCatDatabase
  , query
  , checkDatabase
  , checkDatabase'
  )
where

import Control.Monad
import qualified Data.Array.Unboxed as A
import Data.Bifunctor
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Coerce
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Numeric

-- | General category database.
--   Conceptually this is a sorted array of ascending, non-overlapping inclusive codepoint ranges,
--   with a 'GeneralCategory' attached to each of them.
--   Note that 'NotAssigned' should not be present in this array.
--
--   Internally every element is packed into 'Word64', for the least significant bits:
--
--   * @0-23@ bits represent the low codepoint range (inclusive)
--   * @24-47@ bits represent the high codepoint range (inclusive)
--   * @47-63@ bits represent general category values consistent to 'GeneralCategory'\'s 'Enum' instance.
newtype GenCatDatabase
  = GenCatDatabase (A.UArray Int Word64)
  deriving (Binary)

type Range =
  Either
    (Int, Int) -- [l .. r] (both inclusive)
    Int

-- | General category abbreviations
genCatLitTable :: M.Map T.Text GeneralCategory
genCatLitTable = M.fromList $ zip (T.words abbrs) [minBound .. maxBound]
  where
    abbrs =
      "Lu Ll Lt Lm Lo \
      \Mn Mc Me \
      \Nd Nl No \
      \Pc Pd Ps Pe Pi Pf Po \
      \Sm Sc Sk So \
      \Zs Zl Zp \
      \Cc Cf Cs Co Cn"

verifyUnicodeDataAndProcess :: BSL.ByteString -> Either String [(Range, GeneralCategory)]
verifyUnicodeDataAndProcess raw = do
  let dieIf flag reason =
        when flag $
          Left reason
      rawLines = T.lines . decodeUtf8 . BSL.toStrict $ raw
      rows = fmap extract rawLines
        where
          extract rawLine = (code :: Int, desc, gc)
            where
              [(code, "")] = readHex (T.unpack rawCode)
              rawCode : desc : gc : _ = T.splitOn ";" rawLine
      groupped :: [(T.Text, Either (Int, Int) Int)]
      groupped = norm <$> groupBy zCmp rows
        where
          norm [(c, _, gc)] = (gc, Right c)
          norm [(c0, _, gc0), (c1, _, gc1)]
            | gc0 == gc1
                && T.dropEnd (T.length "First>") gc0
                == T.dropEnd (T.length "Last>") gc1 =
              (gc0, Left (c0, c1))
          norm _ = error "invalid"
          zCmp (_, desc0, _) (_, desc1, _) =
            "First>" `T.isSuffixOf` desc0
              && "Last>" `T.isSuffixOf` desc1
      gpMinus (Left (_a, b)) (Left (c, _d)) = b - c
      gpMinus (Left (_a, b)) (Right c) = b - c
      gpMinus (Right a) (Left (b, _c)) = a - b
      gpMinus (Right a) (Right b) = a - b
      isIncr = and $ zipWith isStrictIncr gs (tail gs)
        where
          isStrictIncr l r = gpMinus l r < 0
          gs = fmap snd groupped
  dieIf
    (not isIncr)
    "Data rows are not strictly ascending."
  let gcGroupped :: [(T.Text, [Either (Int, Int) Int])]
      gcGroupped =
        (\ts -> (fst . head $ ts, fmap snd ts)) <$> groupBy ((==) `on` fst) groupped
      merge acc [] = reverse acc
      merge [] (x : xs) = merge [x] xs
      merge (u : us) (x : xs) = case (u, x) of
        (Left (a, b), Left (c, d)) ->
          if b + 1 == c then merge (Left (a, d) : us) xs else merge (x : u : us) xs
        (Left (a, b), Right c) ->
          if b + 1 == c then merge (Left (a, c) : us) xs else merge (x : u : us) xs
        (Right a, Left (b, c)) ->
          if a + 1 == b then merge (Left (a, c) : us) xs else merge (x : u : us) xs
        (Right a, Right b) ->
          if a + 1 == b then merge (Left (a, b) : us) xs else merge (x : u : us) xs
      gcGroupped' :: [(T.Text, [Either (Int, Int) Int])]
      gcGroupped' = (fmap . second) (merge []) gcGroupped
  dieIf
    (S.member "Cn" (S.fromList $ fmap fst gcGroupped'))
    "No character should be in 'Cn' category"
  pure $ concatMap (\(gc, xs) -> [(x, genCatLitTable M.! gc) | x <- xs]) gcGroupped'

mkDatabase' :: [(Range, GeneralCategory)] -> GenCatDatabase
mkDatabase' gs = GenCatDatabase $ A.listArray (0, l -1) (fmap mkItem gs)
  where
    l = length gs
    mkItem (range, gc) =
      packTuple
        ( fromIntegral lo
        , fromIntegral hi
        , fromIntegral $ fromEnum gc
        )
      where
        (lo, hi) = case range of
          Left (a, b) -> (a, b)
          Right v -> (v, v)

-- | Parses a lazy 'BSL.ByteString' from a __UnicodeData.txt__.
--   For example, content of [UnicodeData.txt](https://www.unicode.org/Public/13.0.0/ucd/UnicodeData.txt)
mkDatabaseFromUnicodeData :: BSL.ByteString -> Either String GenCatDatabase
mkDatabaseFromUnicodeData = verifyUnicodeDataAndProcess >=> pure . mkDatabase'

{-
  low: 0~23
  high: 24~47
  gc: 48~
 -}
packTuple :: (Word32, Word32, Word8) -> Word64
packTuple (lo, high, gc) = fromIntegral lo .|. high' .|. gc'
  where
    high' = fromIntegral high `unsafeShiftL` 24
    gc' = fromIntegral gc `unsafeShiftL` 48

unpackTuple :: Word64 -> (Word32, Word32, Word8)
unpackTuple payload = (lo, high, gc)
  where
    lo, high :: Word32
    lo = fromIntegral (0xFF_FFFF .&. payload)
    high = fromIntegral (0xFF_FFFF .&. (payload `unsafeShiftR` 24))
    gc = fromIntegral (0xFF .&. (payload `unsafeShiftR` 48))

-- | Queries database. @query db@ should be a function equivalent to 'Data.Char.generalCategory',
--   but queries the argument database instead.
query :: GenCatDatabase -> Char -> GeneralCategory
query (GenCatDatabase arr) ch = toEnum . fromIntegral $ search lo hi
  where
    needle :: Word32
    needle = fromIntegral $ ord ch
    (lo, hi) = A.bounds arr
    search l r =
      if l <= r
        then
          let mid = (l + r) `quot` 2
              (rangeL, rangeR, val) = unpackTuple (arr A.! mid)
           in if
                  | needle < rangeL -> search l (mid -1)
                  | needle > rangeR -> search (mid + 1) r
                  | rangeL <= needle && needle <= rangeR -> val
                  | otherwise -> error "unreachable"
        else fromIntegral $ fromEnum NotAssigned

-- | Verifies that all properties of a 'GenCatDatabase' holds,
--   and turns an 'A.UArray' into a database if all requirements are met.
checkDatabase :: A.UArray Int Word64 -> Either String GenCatDatabase
checkDatabase arr = do
  let xs = A.elems arr
  ys <- forM (zip [0 :: Int ..] xs) $ \(i, payload) -> do
    let dieIf tt msg = when tt $ Left $ "failed at element " <> show i <> ": " <> msg
        (lo, hi, val) = unpackTuple payload
    dieIf (not (0 <= lo && lo <= 0x10FFFF)) "low bound out of range"
    dieIf (not (0 <= hi && hi <= 0x10FFFF)) "high bound out of range"
    dieIf (lo > hi) "violates low <= high"
    do
      let gcLo = fromIntegral $ fromEnum (minBound :: GeneralCategory)
          gcHi = fromIntegral $ fromEnum (maxBound :: GeneralCategory)
          notAssn = fromIntegral $ fromEnum NotAssigned
      dieIf (not (gcLo <= val && val <= gcHi)) "general category value out of range"
      dieIf (val == notAssn) "value should not be NotAssigned"
    pure (i, (lo, hi))
  forM_ (zip ys (tail ys)) $ \((i, (_, a)), (j, (b, _))) -> do
    when (a >= b) $
      Left $ "failed when comparing element pair " <> show (i, j) <> "not strictly ascending."
  pure (GenCatDatabase arr)

-- | Verifies that all properties of a 'GenCatDatabase' holds.
checkDatabase' :: GenCatDatabase -> Either String GenCatDatabase
checkDatabase' = coerce checkDatabase
