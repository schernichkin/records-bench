{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Composite basically reuses Vinyl's functionality
module Records.Bench.Composite where

import           Composite.Record
import           Control.Lens
import           Data.Proxy
import qualified Data.Vinyl          as V
import           Records.Bench.Class

{-# INLINE get #-}
get :: (RElem (s :-> a) rs) => proxy (s :-> a) -> Record rs -> a
get = view . rlens

type A0 = "a0" :-> Int
type A1 = "a1" :-> Int
type A2 = "a2" :-> Int
type A3 = "a3" :-> Int
type A4 = "a4" :-> Int
type A5 = "a5" :-> Int
type A6 = "a6" :-> Int
type A7 = "a7" :-> Int
type A8 = "a8" :-> Int
type A9 = "a9" :-> Int

a0 :: Proxy A0
a0 = Proxy

a1 :: Proxy A1
a1 = Proxy

a2 :: Proxy A2
a2 = Proxy

a3 :: Proxy A3
a3 = Proxy

a4 :: Proxy A4
a4 = Proxy

a5 :: Proxy A5
a5 = Proxy

a6 :: Proxy A6
a6 = Proxy

a7 :: Proxy A7
a7 = Proxy

a8 :: Proxy A8
a8 = Proxy

a9 :: Proxy A9
a9 = Proxy

type CompositeRecord = Record [ A1, A2, A3, A4, A5, A6, A7, A8 ]
type CompositeRecordA = Record [ A1, A2, A3, A4, A5, A6, A7, A8, A9 ]
type CompositeRecordP = Record [ A0, A1, A2, A3, A4, A5, A6, A7, A8 ]

instance Accessible CompositeRecord where
  type Tuple CompositeRecord = Tuple8

  {-# INLINE first #-}
  first = get a1

  {-# INLINE last #-}
  last = get a8

  {-# INLINE all #-}
  all r = ( get a1 r, get a2 r, get a3 r, get a4 r
          , get a5 r, get a6 r, get a7 r, get a8 r
          )

instance Accessible CompositeRecordA where
  type Tuple CompositeRecordA = Tuple9

  {-# INLINE first #-}
  first = get a1

  {-# INLINE last #-}
  last = get a9

  {-# INLINE all #-}
  all r = ( get a1 r, get a2 r, get a3 r, get a4 r
          , get a5 r, get a6 r, get a7 r, get a8 r, get a9 r
          )

instance Accessible CompositeRecordP where
  type Tuple CompositeRecordP = Tuple9

  {-# INLINE first #-}
  first = get a0

  {-# INLINE last #-}
  last = get a8

  {-# INLINE all #-}
  all r = ( get a0 r, get a1 r, get a2 r, get a3 r, get a4 r
          , get a5 r, get a6 r, get a7 r, get a8 r
          )

instance Extendable CompositeRecord where
  type Prepended CompositeRecord = CompositeRecordP
  type Appended CompositeRecord = CompositeRecordA

  {-# INLINE prepend #-}
  prepend x xs = x :*: xs

  {-# INLINE append #-}
  append x xs = xs V.<+> (x :*: RNil)
  -- ^ Since Composite does not provide dedicated fuctionality to append value
  -- to the end of record I use native Vinyl's function.

newCompositeRecord :: CompositeRecord
newCompositeRecord = 0 :*: 0 :*: 0 :*: 0 :*: 0 :*: 0 :*: 0 :*: 0 :*: RNil
