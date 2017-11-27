{-# LANGUAGE TypeFamilies #-}

module Records.Bench.Class where

type Tuple8 = (Int, Int, Int, Int, Int, Int, Int, Int)
type Tuple9 = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

class Accessible a where
  type Tuple a

  first :: a -> Int
  last  :: a -> Int
  all   :: a -> Tuple a

class Extendable a where
  type Prepended a
  type Appended a

  prepend :: Int -> a -> Prepended a
  append :: Int -> a -> Appended a
