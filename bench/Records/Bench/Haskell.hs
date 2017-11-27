{-# LANGUAGE TypeFamilies #-}

module Records.Bench.Haskell where

import           Records.Bench.Class

data HaskellRecord = HaskellRecord !Int !Int !Int !Int !Int !Int !Int !Int

data HaskellRecordA = HaskellRecordA !Int !Int !Int !Int !Int !Int !Int !Int !Int

data HaskellRecordP = HaskellRecordP !Int !HaskellRecord

instance Accessible HaskellRecord where
  type Tuple HaskellRecord = Tuple8

  {-# INLINE first #-}
  first (HaskellRecord a1 _ _ _ _ _ _ _) = a1

  {-# INLINE last #-}
  last (HaskellRecord _ _ _ _ _ _ _ a8) = a8

  {-# INLINE all #-}
  all (HaskellRecord a1 a2 a3 a4 a5 a6 a7 a8) = (a1, a2, a3, a4, a5, a6, a7, a8)

instance Accessible HaskellRecordA where
  type Tuple HaskellRecordA = Tuple9

  {-# INLINE first #-}
  first (HaskellRecordA a1 _ _ _ _ _ _ _ _) = a1

  {-# INLINE last #-}
  last (HaskellRecordA _ _ _ _ _ _ _ _ a9) = a9

  {-# INLINE all #-}
  all (HaskellRecordA a1 a2 a3 a4 a5 a6 a7 a8 a9) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance Accessible HaskellRecordP where
  type Tuple HaskellRecordP = Tuple9

  {-# INLINE first #-}
  first (HaskellRecordP a0 _) = a0

  {-# INLINE last #-}
  last (HaskellRecordP _ (HaskellRecord _ _ _ _ _ _ _ a8)) = a8

  {-# INLINE all #-}
  all (HaskellRecordP a0 (HaskellRecord a1 a2 a3 a4 a5 a6 a7 a8)) = (a0, a1, a2, a3, a4, a5, a6, a7, a8)

instance Extendable HaskellRecord where
  type Prepended HaskellRecord = HaskellRecordP
  type Appended HaskellRecord = HaskellRecordA

  {-# INLINE prepend #-}
  prepend = HaskellRecordP

  {-# INLINE append #-}
  append a0 (HaskellRecord a1 a2 a3 a4 a5 a6 a7 a8) = HaskellRecordA a0 a1 a2 a3 a4 a5 a6 a7 a8

newHaskellRecord :: HaskellRecord
newHaskellRecord = HaskellRecord 0 0 0 0 0 0 0 0
