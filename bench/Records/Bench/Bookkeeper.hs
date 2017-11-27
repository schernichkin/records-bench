{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- Bookkeeper failed to compile

module Records.Bench.Bookkeeper where

import           Bookkeeper
import           Records.Bench.Class

type BookkeeperRecord = Book '[ "a1" :=> Int
                              , "a2" :=> Int
                              , "a3" :=> Int
                              , "a4" :=> Int
                              , "a5" :=> Int
                              , "a6" :=> Int
                              , "a7" :=> Int
                              , "a8" :=> Int
                              ]

type BookkeeperRecordA = Book '[ "a1" :=> Int
                               , "a2" :=> Int
                               , "a3" :=> Int
                               , "a4" :=> Int
                               , "a5" :=> Int
                               , "a6" :=> Int
                               , "a7" :=> Int
                               , "a8" :=> Int
                               , "a9" :=> Int
                               ]

type BookkeeperRecordP = Book '[ "a0" :=> Int
                               , "a1" :=> Int
                               , "a2" :=> Int
                               , "a3" :=> Int
                               , "a4" :=> Int
                               , "a5" :=> Int
                               , "a6" :=> Int
                               , "a7" :=> Int
                               , "a8" :=> Int
                               ]

newtype BookkeeperRecordWrapper = BookkeeperRecordWrapper BookkeeperRecord

newtype BookkeeperRecordWrapperA = BookkeeperRecordWrapperA BookkeeperRecordA

newtype BookkeeperRecordWrapperP = BookkeeperRecordWrapperP BookkeeperRecordP

instance Accessible BookkeeperRecordWrapper where
  type Tuple BookkeeperRecordWrapper = Tuple8

  {-# INLINE first #-}
  first (BookkeeperRecordWrapper x) = x ?: #a1

  {-# INLINE last #-}
  last (BookkeeperRecordWrapper x) = x ?: #a8

  {-# INLINE all #-}
  all (BookkeeperRecordWrapper x) = ( x ?: #a1, x ?: #a2, x ?: #a3, x ?: #a4
                                    , x ?: #a5, x ?: #a6, x ?: #a7, x ?: #a8 )

instance Accessible BookkeeperRecordWrapperA where
  type Tuple BookkeeperRecordWrapperA = Tuple9

  {-# INLINE first #-}
  first (BookkeeperRecordWrapperA x) = x ?: #a1

  {-# INLINE last #-}
  last (BookkeeperRecordWrapperA x) = x ?: #a9

  {-# INLINE all #-}
  all (BookkeeperRecordWrapperA x) = ( x ?: #a1, x ?: #a2, x ?: #a3, x ?: #a4
                                     , x ?: #a5, x ?: #a6, x ?: #a7, x ?: #a8,  x ?: #a9 )

instance Accessible BookkeeperRecordWrapperP where
  type Tuple BookkeeperRecordWrapperP = Tuple9

  {-# INLINE first #-}
  first (BookkeeperRecordWrapperP x) = x ?: #a0

  {-# INLINE last #-}
  last (BookkeeperRecordWrapperP x) = x ?: #a8

  {-# INLINE all #-}
  all (BookkeeperRecordWrapperP x) = ( x ?: #a0, x ?: #a1, x ?: #a2, x ?: #a3, x ?: #a4
                                     , x ?: #a5, x ?: #a6, x ?: #a7, x ?: #a8 )

instance Extendable BookkeeperRecordWrapper where
  type Prepended BookkeeperRecordWrapper = BookkeeperRecordWrapperP
  type Appended BookkeeperRecordWrapper = BookkeeperRecordWrapperA

  {-# INLINE prepend #-}
  prepend x (BookkeeperRecordWrapper xs) = BookkeeperRecordWrapperP (#a0 =: x $ xs)

  {-# INLINE append #-}
  append x (BookkeeperRecordWrapper xs) = BookkeeperRecordWrapperA $ xs & #a9 =: x

newBookkeeperRecord :: BookkeeperRecordWrapper
newBookkeeperRecord = BookkeeperRecordWrapper $ emptyBook
  & #a1 =: 0
  & #a2 =: 0
  & #a3 =: 0
  & #a4 =: 0
  & #a5 =: 0
  & #a6 =: 0
  & #a7 =: 0
  & #a8 =: 0
