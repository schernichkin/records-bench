{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module Records.Bench.Suite where

import           Control.Monad.Par.Class
import           Criterion.Main
import           Records.Bench.Class     as RB

data RecordSpec = forall a . ( Accessible a
                             , Extendable a
                             , Accessible (Prepended a)
                             , Accessible (Appended a)
                             , NFData (Tuple a)
                             , NFData (Tuple (Prepended a))
                             , NFData (Tuple (Appended a))
                             ) => RecordSpec !String !a

benchSuite :: [RecordSpec] -> [Benchmark]
benchSuite xs =
  [ bgroup "first" $ map (\(RecordSpec name a) -> bench name $ nf first a) xs
  , bgroup "prepend-first" $ map (\(RecordSpec name a) -> bench name $ nf (first . prepend 0) a) xs
  , bgroup "append-first" $ map (\(RecordSpec name a) -> bench name $ nf (first . append 0) a) xs
  , bgroup "last" $ map (\(RecordSpec name a) -> bench name $ nf RB.last a) xs
  , bgroup "prepend-last" $ map (\(RecordSpec name a) -> bench name $ nf (RB.last . prepend 0) a) xs
  , bgroup "append-last" $ map (\(RecordSpec name a) -> bench name $ nf (RB.last . append 0) a) xs
  , bgroup "all" $ map (\(RecordSpec name a) -> bench name $ nf RB.all a) xs
  , bgroup "prepend-all" $ map (\(RecordSpec name a) -> bench name $ nf (RB.all . prepend 0) a) xs
  , bgroup "append-all" $ map (\(RecordSpec name a) -> bench name $ nf (RB.all . append 0) a) xs
  ]
