module Bench
  ( main
  )
where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import qualified Data.Char as BaseChar
import qualified Data.Char.GeneralCategory.V13_0_0 as U13
import Data.Word
import System.Random.Shuffle

allChars :: [] Char
allChars = [minBound .. maxBound]

allPlane0 :: [] Char
allPlane0 = ['\0' .. '\xFFFF']

benchmarkWithChars :: [] Char -> (Char -> BaseChar.GeneralCategory) -> Word8
benchmarkWithChars xs gc = foldr (\ch -> (f ch `xor`)) 0 xs
  where
    f = fromIntegral . fromEnum . gc

randomlyShuffled :: [a] -> Int -> IO [a]
randomlyShuffled xsPre limit = do
  let xs = take limit xsPre
      l = length xs
  shuffleM xs

gcBenchGroup :: String -> [Char] -> Benchmark
gcBenchGroup tag inp =
  bgroup
    tag
    [ bench "base" $
        nf fn BaseChar.generalCategory
    , bench "GeneralCategory.V13_0_0" $
        nf fn U13.generalCategory
    ]
  where
    fn = mkFn inp benchmarkWithChars
    mkFn xs f = xs `deepseq` f xs

main :: IO ()
main =
  defaultMain
    [ gcBenchGroup "all chars, sequential" allChars
    , gcBenchGroup "plane0, sequentual" allPlane0
    , env
        (randomlyShuffled allChars 10000)
        (gcBenchGroup "full range, 1000 shuffled elements")
    , env
        (randomlyShuffled allPlane0 10000)
        (gcBenchGroup "plane0, 1000 shuffled elements")
    ]
