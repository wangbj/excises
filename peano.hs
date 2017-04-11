{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data Nil
data Cons x xs

class First list x | list -> x

instance First Nil Nil
instance First (Cons x xs) x

class ListConcat a b c | a b -> c
instance ListConcat Nil x x
instance ListConcat as bs cs => ListConcat (Cons a as) bs (Cons a cs)

class ListConcatAll ls l | ls -> l
instance ListConcatAll Nil Nil
instance (ListConcat chunk acc result, ListConcatAll rest acc) =>
  ListConcatAll (Cons chunk rest) result

data False
data True

class AnyTrue list t | list -> t
instance AnyTrue Nil False
instance AnyTrue (Cons True more) True
instance (AnyTrue list t) => AnyTrue (Cons False list) t

class Not b1 b | b1 -> b
instance Not True False
instance Not False True

class Or b1 b2 b | b1 b2 -> b
instance Or True True True
instance Or False True True
instance Or True False True
instance Or False False False

class And b1 b2 b | b1 b2 -> b
instance And True True True
instance And True False False
instance And False True False
instance And False False False

data Z
data S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

class PeanoEqual a b t | a b -> t
instance PeanoEqual Z Z True
instance PeanoEqual (S a) Z False
instance PeanoEqual Z (S b) False
instance (PeanoEqual a b t) => PeanoEqual (S a) (S b) t

class PeanoLT a b t | a b -> t
instance PeanoLT Z Z False
instance PeanoLT (S a) Z False
instance PeanoLT Z (S b) True
instance PeanoLT a b t =>
  PeanoLT (S a) (S b) t

class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff Z Z Z
instance PeanoAbsDiff (S a) Z (S a)
instance PeanoAbsDiff Z (S b) (S b)
instance PeanoAbsDiff a b c => PeanoAbsDiff (S a) (S b) c

class Range n xs | n -> xs
instance Range Z Nil
instance (Range n xs) => Range (S n) (Cons n xs)

class PeanoAdd x y z | x y -> z

instance PeanoAdd Z y y
instance PeanoAdd x y z => PeanoAdd (S x) y (S z)

class Add2 a b c | a b -> c where
  add2 :: a -> b -> c
instance PeanoAdd x y z => Add2 x y z where add2 = undefined

class PeanoSub x y z | x y -> z
instance PeanoSub Z Z Z
instance PeanoSub (S x) Z (S x)
instance ( PeanoLT y x r1,
           PeanoEqual y x r2,
           Or r1 r2 True,
           PeanoAbsDiff x y r4
           ) => PeanoSub x y r4
           
class Sub2 x y z | x y -> z where
  sub2 :: x -> y -> z

instance (PeanoSub x y z) => Sub2 x y z where sub2 = undefined

class PeanoMul x y z | x y -> z

instance PeanoMul Z b Z
instance PeanoMul a Z Z
instance (PeanoMul a b r1,
          PeanoAdd b r1 r
         ) => PeanoMul (S a) b r

class Mul2 a b c | a b -> c where
  mul2 :: a -> b -> c

instance PeanoMul a b c => Mul2 a b c where mul2 = undefined
