{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Records.Bench.Vinyl where

import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Records.Bench.Class

{-# INLINE get #-}
get :: RElem '(s, t) rs (RIndex '(s, t) rs) => sing '(s, t) -> Rec ElField rs -> t
get l = getField . rget l

type A0 = '( "a0", Int )
type A1 = '( "a1", Int )
type A2 = '( "a2", Int )
type A3 = '( "a3", Int )
type A4 = '( "a4", Int )
type A5 = '( "a5", Int )
type A6 = '( "a6", Int )
type A7 = '( "a7", Int )
type A8 = '( "a8", Int )
type A9 = '( "a9", Int )

a0 :: SField A0
a0 = SField

a1 :: SField A1
a1 = SField

a2 :: SField A2
a2 = SField

a3 :: SField A3
a3 = SField

a4 :: SField A4
a4 = SField

a5 :: SField A5
a5 = SField

a6 :: SField A6
a6 = SField

a7 :: SField A7
a7 = SField

a8 :: SField A8
a8 = SField

a9 :: SField A9
a9 = SField

type VynilRecord = FieldRec '[ A1, A2, A3, A4, A5, A6, A7, A8 ]

type VynilRecordA = FieldRec '[ A1,  A2, A3, A4, A5, A6, A7, A8, A9 ]

type VynilRecordP = FieldRec '[ A0, A1,  A2, A3, A4, A5, A6, A7, A8 ]

instance Accessible VynilRecord where
  type Tuple VynilRecord = Tuple8

  {-# INLINE first #-}
  first = get a1

  {-# INLINE last #-}
  last  = get a8

  {-# INLINE all #-}
  all x = ( get a1 x, get a2 x, get a3 x, get a4 x
          , get a5 x, get a6 x, get a7 x, get a8 x
          )

instance Accessible VynilRecordA where
  type Tuple VynilRecordA = Tuple9

  {-# INLINE first #-}
  first = get a1

  {-# INLINE last #-}
  last  = get a9

  {-# INLINE all #-}
  all x = ( get a1 x, get a2 x, get a3 x, get a4 x
          , get a5 x, get a6 x, get a7 x, get a8 x
          , get a9 x )

instance Accessible VynilRecordP where
  type Tuple VynilRecordP = Tuple9

  {-# INLINE first #-}
  first = get a0

  {-# INLINE last #-}
  last  = get a8

  {-# INLINE all #-}
  all x = ( get a0 x, get a1 x, get a2 x, get a3 x, get a4 x
          , get a5 x, get a6 x, get a7 x, get a8 x
          )

instance Extendable VynilRecord where
  type Prepended VynilRecord = VynilRecordP
  type Appended VynilRecord = VynilRecordA

  {-# INLINE prepend #-}
  prepend x xs = Field x :& xs

  {-# INLINE append #-}
  append x xs = xs <+> a9 =: x

newVinylRecord :: FieldRec '[ A1, A2, A3, A4, A5, A6, A7, A8 ]
newVinylRecord =  (a1 =: 0)
              <+> (a2 =: 0)
              <+> (a3 =: 0)
              <+> (a4 =: 0)
              <+> (a5 =: 0)
              <+> (a6 =: 0)
              <+> (a7 =: 0)
              <+> (a8 =: 0)

{-
newVinylRecord :: FieldRec '[ A1, A2, A3, A4, A5, A6, A7, A8 ]
newVinylRecord =  (a1 =: 0)
              <+> (a2 =: 0)
              <+> (a3 =: 0)
              <+> (a4 =: 0)
              <+> (a5 =: 0)
              <+> (a6 =: 0)
              <+> (a7 =: 0)
              <+> (a8 =: 0)
-}
