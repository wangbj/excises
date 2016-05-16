{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Vec (
    Vec (VNil, VCons)
  , Lit
    ) where
import           Data.Serialize
import           Control.Applicative
import qualified GHC.TypeLits as Lit

data Nat = Z | S Nat

type family Lit n where
    Lit 0 = 'Z
    Lit n = 'S (Lit (n Lit.- 1))
    
data Vec :: Nat -> * -> * where
  VNil :: Vec 'Z a
  VCons :: a -> Vec n a -> Vec ('S n) a

instance Show (Vec Z a) where
    show _ = ""

instance (Show a, Show (Vec n a)) => Show (Vec ('S n) a) where
    show (VCons x xs) = show x ++ " " ++ show xs

instance (Serialize a) => Serialize (Vec Z a)  where
  get = return VNil
  put _ = return mempty

instance (Serialize a, Serialize (Vec n a)) => Serialize (Vec ('S n) a) where
  get = liftA2 VCons get get
  put (VCons x xs) = put x >> put xs

instance (Eq a) => Eq (Vec n a) where
    VNil == VNil = True
    (VCons x xs) == (VCons y ys) = if x == y then xs == ys else False
