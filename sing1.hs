{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding (head, tail)

data Nat = Z | S Nat
  deriving Show

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: forall n. SNat n -> SNat ('S n)

type family (n :: Nat) :+ (m :: Nat) :: Nat

type instance Z :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family (n :: Nat) :* (m :: Nat) :: Nat

type instance Z :* m = Z
type instance (S n) :* m = m :+ (n :* m)

infixl 6 :+
infixl 7 :*

data Vector a n where
  Nil :: Vector a Z
  Cons :: a -> Vector a n -> Vector a (S n)

deriving instance Eq a => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)

head :: Vector a (S n) -> a
head (Cons x _) = x

tail :: Vector a (S n) -> Vector a n
tail (Cons _ xs) = xs

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

toList :: Vector a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList_ :: SNat n -> [a] -> Vector a n
fromList_ SZ [] = Nil
fromList_ (SS n) (x:xs) = Cons x (fromList_ n xs)

vmap :: (a -> b) -> Vector a n -> Vector b n
vmap f Nil = Nil
vmap f (Cons x xs) = Cons (f x ) (vmap f xs)
