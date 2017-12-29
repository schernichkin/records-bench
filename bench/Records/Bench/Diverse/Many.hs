{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Records.Bench.Diverse.Many where

import           Data.Diverse.Many
import           Data.Diverse.TypeLevel
import           Data.Proxy
import           Data.Tagged
import           Records.Bench.Class

type DMRecord = Many '[ Tagged "a1" Int
                      , Tagged "a2" Int
                      , Tagged "a3" Int
                      , Tagged "a4" Int
                      , Tagged "a5" Int
                      , Tagged "a6" Int
                      , Tagged "a7" Int
                      , Tagged "a8" Int
                      ]

type DMRecordA = Many '[ Tagged "a1" Int
                       , Tagged "a2" Int
                       , Tagged "a3" Int
                       , Tagged "a4" Int
                       , Tagged "a5" Int
                       , Tagged "a6" Int
                       , Tagged "a7" Int
                       , Tagged "a8" Int
                       , Tagged "a9" Int
                       ]

type DMRecordP = Many '[ Tagged "a0" Int
                       , Tagged "a1" Int
                       , Tagged "a2" Int
                       , Tagged "a3" Int
                       , Tagged "a4" Int
                       , Tagged "a5" Int
                       , Tagged "a6" Int
                       , Tagged "a7" Int
                       , Tagged "a8" Int
                       ]

get :: forall l xs proxy x . (UniqueLabelMember l xs, (Tagged l x) ~ KindAtLabel l xs) => proxy l -> Many xs ->  x
get p = untag . fetchL p
-- fetchL' _ (Many xs) = unsafeCoerce $ fetch_ (Proxy @(IndexOf x xs)) xs


instance Accessible DMRecord where
  type Tuple DMRecord = Tuple8

  {-# INLINE first #-}
  first = get @"a1" Proxy

  {-# INLINE last #-}
  last = get @"a8" Proxy

  {-# INLINE all #-}
  all x = ( get @"a1" Proxy x, get @"a2" Proxy x, get @"a3" Proxy x, get @"a4" Proxy x
          , get @"a5" Proxy x, get @"a6" Proxy x, get @"a7" Proxy x, get @"a8" Proxy x
          )

instance Accessible DMRecordA where
  type Tuple DMRecordA = Tuple9

  {-# INLINE first #-}
  first = get @"a1" Proxy

  {-# INLINE last #-}
  last = get @"a9" Proxy

  {-# INLINE all #-}
  all x = ( get @"a1" Proxy x, get @"a2" Proxy x, get @"a3" Proxy x, get @"a4" Proxy x
          , get @"a5" Proxy x, get @"a6" Proxy x, get @"a7" Proxy x, get @"a8" Proxy x
          , get @"a8" Proxy x
          )

instance Accessible DMRecordP where
  type Tuple DMRecordP = Tuple9

  {-# INLINE first #-}
  first = get @"a0" Proxy

  {-# INLINE last #-}
  last = get @"a8" Proxy

  {-# INLINE all #-}
  all x = ( get @"a0" Proxy x
          , get @"a1" Proxy x, get @"a2" Proxy x, get @"a3" Proxy x, get @"a4" Proxy x
          , get @"a5" Proxy x, get @"a6" Proxy x, get @"a7" Proxy x, get @"a8" Proxy x
          )

instance Extendable DMRecord where
  type Prepended DMRecord = DMRecordP
  type Appended DMRecord = DMRecordA

  {-# INLINE prepend #-}
  prepend x xs = Tagged x ./ xs

  {-# INLINE append #-}
  append x xs = xs \. Tagged x

newDMRecord :: DMRecord
newDMRecord = Tagged 0 ./ Tagged 0 ./ Tagged 0 ./ Tagged 0 ./
              Tagged 0 ./ Tagged 0 ./ Tagged 0 ./ Tagged 0 ./ nil
