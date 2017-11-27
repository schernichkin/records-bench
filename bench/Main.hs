module Main where

import           Criterion.Main
import           Records.Bench.Composite
import           Records.Bench.Diverse.Many
import           Records.Bench.Haskell
import           Records.Bench.Suite
import           Records.Bench.Vinyl

main :: IO ()
main = defaultMain $ benchSuite
  [ RecordSpec "haskell" newHaskellRecord
  , RecordSpec "vynil" newVinylRecord
  , RecordSpec "composite" newCompositeRecord
  ]
