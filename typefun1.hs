{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified GHC.TypeLits as TL

data Z
data S n

data True
data False

data a :+: b

class PeanoAdd a b c | a b -> c
instance PeanoAdd Z b b
instance (PeanoAdd a b c) => PeanoAdd (S a) b (S c)

class PeanoEq a b c | a b -> c
instance PeanoEq Z Z True
instance PeanoEq (S a) Z False
instance (PeanoEq a b t) => PeanoEq (S a) (S b) t

class PeanoNe a b c | a b -> c
instance PeanoNe Z Z False
instance PeanoNe (S a) Z True
instance PeanoNe a b t => PeanoNe (S a) (S b) t

class Not a b | a -> b
instance Not False True
instance Not True False

class And a b c | a b -> c
instance And True True True
instance And True False False
instance And False True False
instance And False False False

class Or a b c | a b -> c
instance Or True True True
instance Or True False True
instance Or False True True
instance Or False False False

class PeanoLt a b c | a b -> c
instance PeanoLt (S a) Z False
instance PeanoLt Z (S b) True
instance PeanoLt a b t => PeanoLt (S a) (S b) t

class PeanoLe a b c | a b -> c
instance PeanoLe Z b True
instance PeanoLe (S a) Z False
instance PeanoLe a b t => PeanoLe (S a) (S b) t

class PeanoGt a b c | a b -> c
instance PeanoGt (S a) Z True
instance PeanoGt Z b False
instance PeanoGt a b t => PeanoGt (S a) (S b) t

class PeanoGe a b c | a b -> c
instance PeanoGe a Z True
instance PeanoGe Z (S b) False
instance PeanoGe a b t => PeanoGe (S a) (S b) t

class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff a Z a
instance PeanoAbsDiff Z b b
instance PeanoAbsDiff a b t => PeanoAbsDiff (S a) (S b) t

class PeanoSub a b c | a b -> c
instance PeanoSub a Z a
instance ( PeanoGe a b True
         , PeanoAbsDiff a b r) => PeanoSub a b r

class PeanoMul a b c | a b -> c
instance PeanoMul Z b Z
instance PeanoMul a Z Z
instance ( PeanoMul a b t
         , PeanoAdd b t r) => PeanoMul (S a) b r

-- type level natural number addition, as function
type family a + b where
  Z + b = b
  (S a) + b = S (a + b)

type family a * b where
  Z * b = Z
  (S a) * b = b + (a * b)

type family a < b where
  Z < Z = False
  Z < S b = True
  (S a) < (S b) = a < b

type family a <= b where
  Z <= b = True
  (S a) <= Z = False
  (S a ) <= (S b) = a <= b

type family a > b where
  (S a) > Z = True
  Z > b     = False
  (S a) > (S b) = a > b

type family a >= b where
  a >= Z = True
  Z >= (S b) = False
  (S a) >= (S b) = a >= b

type family a && b where
  True && True = True
  True && False = False
  False && True = False
  False && False = False
  
type family If c t e where
  If True t e = t
  If False t e = e

type family Sub2 a b where
  Sub2 a Z = a
  Sub2 (S a) (S b) = Sub2 a b

type family Div2 a b where
  Div2 Z (S b) = Z
  Div2 (S a) (S b) = If (a >= b) (S (Div2 (Sub2 (S a) (S b)) (S b))) Z

type Div2_ a b c = Div2 a b ~ c

class PeanoDiv a b c | a b -> c

instance ( Div2_ a b c
         , PeanoNe b Z True) => PeanoDiv a b c

class PeanoDiv2 a b c | a b -> c where
  div2 :: a -> b -> c

instance PeanoDiv a b c => PeanoDiv2 a b c where div2 = undefined

type family Mod2 a b where
  Mod2 Z (S b) = Z
  Mod2 (S a) (S b) = If (a >= b) (Mod2 (Sub2 (S a) (S b)) (S b)) (S a)

type Mod2_ a b c = Mod2 a b ~ c

class PeanoMod a b c | a b -> c
instance ( Mod2_ a b c
         , PeanoNe b Z True) => PeanoMod a b c

class PeanoDivMod a b c | a b -> c
instance ( PeanoDiv a b d
         , PeanoMod a b r) => PeanoDivMod a b (d, r)

class PeanoDivMod2 a b c | a b -> c where
  divMod2 :: a -> b -> c

instance PeanoDivMod a b c => PeanoDivMod2 a b c where divMod2 = undefined
-- :type divMod2 @(S (S (S (S (S Z))))) @(S (S Z))
-- divMod2 @(S (S (S (S (S Z))))) @(S (S Z))
--   :: S (S (S (S (S Z)))) -> S (S Z) -> (S (S Z), S Z)

type family FromInteger a where
  FromInteger 0 = Z
  FromInteger (n) = S (FromInteger (n TL.- 1))

type family ToInteger a where
  ToInteger Z = 0
  ToInteger (S n) = 1 TL.+ (ToInteger n)

{-
type family ISqrtIter a b where
  ISqrtIter Z b = Z
  ISqrtIter a b = If (  ( (b * b) <= a) && ( ((S b) * (S b)) > a)  ) b (ISqrtIter a (S b))

type family ISqrt a where
  ISqrt a = ISqrtIter a Z

data Nil
data Cons x xs
-}

--
main = return()
