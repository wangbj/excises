{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}



import qualified Data.ByteString as B
import           Data.ByteString(ByteString)
import           Data.Serialize
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.Either
import           Control.Applicative
import           Control.Monad
import           Control.Arrow
import           Data.Int
import           Data.List
import           Data.Ord
import           ADT
import           Tuple

instance Arbitrary Header where
    arbitrary = (Header 0) <$> dim <*> numVars <*> numTuples <*> avgScores
      where dim = choose (2, 8)
            numVars = choose (1, 1000)
            numTuples = choose (1, 1000)
            avgScores = choose (100.0, 200.0)

means = getMeans . foldl acc (0, 0)
  where acc (!n, !m) x = (succ n, m + fromIntegral x)
        getMeans (!n, !m) = m / (fromIntegral n)

meansAll = getMeans . foldl acc (0, 0)
  where acc (!n, !m) (_, !avg) = (succ n, m + avg)
        getMeans (!n, !m) = m / (fromIntegral n)

instance Arbitrary ContentData where
    arbitrary = do
       dim            <- choose (2, 8)
       numVars        <- choose (1, 1000000000)
       numTuples      <- choose (1, 10)
       let nat        = choose (0, fromIntegral numVars) :: Gen Int32
       let ld         = replicateM dim nat
       let avg        = fmap means ld
       let tuples     = replicateM numTuples (liftA2 (,) ld avg)
       allTuples      <- tuples
       let avgScore  = meansAll allTuples
       return $! ContentData dim numVars numTuples avgScore allTuples

-- Make sure our Arbitrary instance is consistent
prop_consistent_data1 (ContentData dim _ numTuples _ t) = length t == numTuples && all (== dim) (fmap (\(l, _) -> (length l)) t)

prop_content_can_be_serialized :: ContentData -> Bool
prop_content_can_be_serialized c = isRight dec && c' == c
    where enc = encode c
          dec = decode enc :: Either String ContentData
          (Right c') = dec

prop_header_can_be_serialized :: Header -> Bool
prop_header_can_be_serialized c = isRight dec && c' == c
    where enc = encode c
          dec = decode enc :: Either String Header
          (Right c') = dec

prop_sanity_header_is_28B :: Header -> Bool
prop_sanity_header_is_28B h = B.length (encode h) == 28

prop_sanity_double_is_8B :: Double -> Bool
prop_sanity_double_is_8B n = B.length (encode n) == 8

prop_serialize_content :: ContentData -> Bool
prop_serialize_content ctx = isRight dec && dec' == ctx
    where hdr = fromContent ctx
          enc = encodeContent ctx
          dec = decodeContent hdr enc
          (Right dec') = dec

prop_serialize_content_valid_size :: ContentData -> Bool
prop_serialize_content_valid_size ctx@(ContentData dim _ nt _ ts) = B.length enc == (4*dim+8)*nt
    where enc = encodeContent ctx

getTuples (Tuple ts _) = ts

sortNumTopBotBF :: Int -> [Tuple] -> ([Tuple], [Tuple])
sortNumTopBotBF n ts = (top, bot)
    where top = take n (sortBy (comparing Down) ts)
          bot = take n (sort ts)

showts :: [Tuple] -> IO ()
showts = mapM_ print

showtts :: ([Tuple], [Tuple]) -> IO ()
showtts (l, r) = mapM_ print l >> mapM_ print r

sortNumTopBot :: Int -> [Tuple] -> ([Tuple], [Tuple])
sortNumTopBot n = fmt . foldl acc (TopSorted n 0 [], BotSorted n 0 []) 
    where acc (!tops, !bots) x = (insertTop x tops, insertBot x bots)
          fmt (TopSorted n k t, BotSorted n' k' b) = (reverse t, reverse b)

data SortTuples = SortTuples Int Tuples deriving Show

instance Arbitrary SortTuples where
    arbitrary = liftA2 SortTuples (choose (0, 100)) arbitrary

prop_tuple_sorted_properly :: SortTuples -> Bool
prop_tuple_sorted_properly (SortTuples n (Tuples ts)) = sortNumTopBotBF n ts == sortNumTopBot n ts

-- run QuickCheck tests.

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >> return ()
