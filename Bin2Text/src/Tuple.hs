{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Tuple (
      Tuple(..)
    , Tuples(..)
    , TopSortedTuples(..)
    , BotSortedTuples(..)
    , emptyTopSorted
    , emptyBotSorted
    , tupleScore
    , insertTopIf
    , insertBotIf
    , insertTop
    , insertBot
    ) where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.List
import           Data.Ord
import           Text.Printf
import           Test.QuickCheck
import           Control.Applicative
import           Control.Monad
import           Data.Monoid

data Tuple = Tuple ![Int32] {-# UNPACK #-} !Double deriving (Eq)

tupleScore (Tuple _ scr) = scr

means = getMeans . foldl acc (0, 0)
  where acc (!n, !s) x = (succ n, s + fromIntegral x)
        getMeans (!n, !s) = s / (fromIntegral n)

newtype Tuples = Tuples [Tuple]

instance Show Tuples where
    show (Tuples ts) = intercalate "\n" (map show ts)

instance Arbitrary Tuple where
    arbitrary = do
        len <- choose (2, 8)
        ts <- fmap (take len) (infiniteListOf (choose (0, 1000000)))
        return $! Tuple ts (means ts)

instance Arbitrary Tuples where
    arbitrary = do
        dim <- choose (2, 8)
        len <- choose (0, 1000)
        let ns  = fmap (take dim) (infiniteListOf (choose (0, 1000000)))
        let t   =  liftA2 Tuple ns (fmap means ns)
        ts  <- replicateM len t
        return $! Tuples ts

instance Show Tuple where
    show (Tuple ts score) = concat (map (\x -> show x ++ "\t") ts ++ [printf "%.10f" score])

instance Ord Tuple where
    compare (Tuple xs x) (Tuple ys y) = compare x y

data TopSortedTuples = TopSorted {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![Tuple]
data BotSortedTuples = BotSorted {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![Tuple]

emptyTopSorted :: Int -> TopSortedTuples
emptyBotSorted :: Int -> BotSortedTuples
emptyTopSorted n = TopSorted n 0 []
emptyBotSorted n = BotSorted n 0 []

instance Show TopSortedTuples where
    show (TopSorted _ _ ts) = concatMap (\x -> show x ++ "\n") ts

instance Show BotSortedTuples where
    show (BotSorted _ _ ts) = init $ concatMap (\x -> show x ++ "\n") (ts)

insertTopIf :: (Tuple -> Bool) -> Tuple -> TopSortedTuples -> TopSortedTuples
insertTopIf pred t@(Tuple ns score) res@(TopSorted cap _ [])
    | cap   ==      0 = res
    | pred t == False = res
    | otherwise       = TopSorted cap 1 [t]
insertTopIf pred t@(Tuple ns score) res@(TopSorted cap n ts@(x@(Tuple ns1 score1):xs))
    | cap   ==      0 = TopSorted 0 0 []
    | pred t == False = res
    | n     <     cap = TopSorted cap (succ n) (insert t ts)
    | score >  score1 = TopSorted cap n (insert t xs)
    | score <= score1 = res

insertTop p t = insertTopIf (const True) p t

insertBotIf :: (Tuple -> Bool) -> Tuple -> BotSortedTuples -> BotSortedTuples
insertBotIf pred t@(Tuple ns score) res@(BotSorted cap _ [])
    | cap   ==      0 = res
    | pred t == False = res
    | otherwise       = BotSorted cap 1 [t]
insertBotIf pred t@(Tuple ns score) res@(BotSorted cap n ts@(x@(Tuple ns1 score1):xs))
    | cap   ==      0 = BotSorted 0 0 []
    | pred t == False = res      
    | n     <     cap = BotSorted cap (succ n) (insertBy (flip (comparing getScore)) t ts)
    | score <  score1 = BotSorted cap n (insertBy (flip (comparing getScore)) t xs)
    | score >= score1 = res
  where getScore (Tuple _ scr) = scr

insertBot p t = insertBotIf (const True) p t
