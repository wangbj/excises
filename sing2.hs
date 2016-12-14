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
{-# LANGUAGE TemplateHaskell #-}

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH

import Prelude hiding (head, tail)

$(singletons [d|
               data Nat = Z | S Nat
               pred :: Nat -> Nat
               pred Z = Z
               pred (S n) = n
               |])

data Vector a n where
  Nil :: Vector a Z
  Cons :: a -> Vector a n -> Vector a (sS n)

deriving instance (Show a) => Show (Vector a n)

fromList_ :: SNat n -> [a] -> Vector a n
fromList_ SZ _ = Nil
fromList_ (SS n) (x:xs) = Cons x (fromList_ n xs)

fromList :: SingI n => [a] -> Vector a n
fromList = fromList_ sing

