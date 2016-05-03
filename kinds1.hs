{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import Data.Serialize   -- require cereal
import Control.Applicative
import GHC.TypeLits

data N = Z | S N

data Scalar :: N -> * -> * where
  Nil :: Scalar Z a
  Cons :: a -> Scalar n a -> Scalar (S n) a

instance Show (Scalar Z a) where
    show _ = ""

instance (Show a, Show (Scalar n a)) => Show (Scalar (S n) a) where
    show (Cons x xs) = show x ++ " " ++ show xs

instance (Serialize a) => Serialize (Scalar Z a)  where
  get = return Nil
  put _ = return $ mempty

instance (Serialize a, Serialize (Scalar n a)) => Serialize (Scalar (S n) a) where
  get = liftA2 Cons get get
  put (Cons x xs) = put x >> put xs

v1 = Cons 1 Nil                             :: Scalar (S Z) Int
v2 = Cons 1 (Cons 2 Nil)                    :: Scalar (S (S Z)) Int
v3 = Cons 1 (Cons 2 (Cons 3 Nil))           :: Scalar (S (S (S Z))) Int
v4 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))  :: Scalar (S (S (S (S Z)))) Int

main = return ()
