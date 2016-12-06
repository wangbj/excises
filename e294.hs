{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Data.Proxy
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client (newManager)
import           Servant.API
import           Servant.Client
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Data.Char
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict (IntMap)
import           Data.Maybe
import           Data.Function
import           Control.Monad.Reader
import           Control.Monad
import           Test.Hspec
import           Test.Hspec.Core.Spec

type API = Get '[OctetStream] ByteString

api :: Proxy API
api = Proxy

type Dict = [Text]

getDict :: IO (Either ServantError Dict)
getDict =  do
  manager <- newManager tlsManagerSettings
  res <- runClientM (client api) (ClientEnv manager (BaseUrl Https "storage.googleapis.com" 443 "/google-code-archive-downloads/v2/code.google.com/dotnetperls-controls/enable1.txt"))
  return $ Text.splitOn "\r\n" . Text.decodeUtf8 <$> res

-- ^ convert text to intmap for fast processing
fromList :: Text -> IntMap Int
fromList = IntMap.fromListWith (+) . flip zip (repeat 1) . xlate
  where
    xlate :: Text -> [Int]    -- ^ treat '?' as a special char.
    xlate = map (\x -> if x == '?' then maxBound else ord x) . Text.unpack

scrabbleIt :: IntMap Int -> IntMap Int -> Bool
scrabbleIt tiles word
  | IntMap.null word  = True
  | IntMap.null tiles = False
scrabbleIt (IntMap.minViewWithKey -> Just ((k, v), m)) (IntMap.minViewWithKey -> Just ((k', v'), m')) = scrabbleHelper k v m k' v' m'
scrabbleHelper k v m k' v' m'
  | k == maxBound = scrabbleIt (if v <= v' then m else IntMap.insert k (v - v') m)
                    (if v < v' then IntMap.insert k (v' - v) m' else m')
  | k < k'        = scrabbleIt m (IntMap.insert k' v' m') 
  | k > k'        = case IntMap.lookup maxBound m >> IntMap.maxViewWithKey (IntMap.insert k v m) of
      Nothing           -> False
      Just ((k1,v1),m1) -> scrabbleHelper k1 v1 m1 k' v' m'
  | v >= v'       = scrabbleIt m m'
  | v  < v'       = scrabbleIt m (IntMap.insert k' (v' - v) m')
  
scrabble :: Text -> Text -> Bool
scrabble = on scrabbleIt fromList

matchLongest :: Text -> Text -> Text -> Text
matchLongest tile res word
  | scrabble tile word == False = res
  | Text.length word  > Text.length res = word
  | Text.length word <= Text.length res = res

longest :: Text -> ReaderT Dict IO Text
longest tile = asks (foldl (matchLongest tile) Text.empty)

points = concat $ [
    zip "eaionrtlsu" (repeat 1)
  , zip "dg" (repeat 2)
  , zip "bcmp" (repeat 3)
  , zip "fhvwy" (repeat 4)
  , zip "k" (repeat 5)
  , zip "jx" (repeat 8)
  , zip "qz" (repeat 10) ]

getPoints :: Text -> Text -> Int
getPoints t = cnt . on (IntMap.intersectionWith min) fromList t
  where
    cnt = IntMap.foldlWithKey acc 0
    acc pts k v = pts + v * fromMaybe 0 (lookup (chr k) points)

matchHighest :: Text -> (Int, Text) -> Text -> (Int, Text)
matchHighest tile res@(p, t) word
  | scrabble tile word == False = res
  | pts                 > p     = (pts, word)
  | pts                <= p     = res
  where pts = getPoints tile word

highest :: Text -> ReaderT Dict IO Text
highest tile = asks (snd . foldl (matchHighest tile) (0, ""))

example_testcases = [ ( ("ladilmy", "daily"), True)
                    , ( ("eerriin", "eerie"), False)
                    , ( ("orrpgma", "program"), True)
                    , ( ("orppgma", "program"), False) ]
bonus1_testcases = [ ( ("pizza??", "pizzazz"), True)
                   , ( ("piizza?", "pizzazz"), False)
                   , ( ("a??????", "program"), True)
                   , ( ("b??????", "program"), False) ]

bonus2_testcases = [ ("dcthoyueorza", "coauthored")
                   , ("uruqrnytrois", "turquois")
                   , ("rryqeiaegicgeo??", "greengrocery")
                   , ("udosjanyuiuebr??", "subordinately")
                   , ("vaakojeaietg????????", "ovolactovegetarian") ]

bonus3_testcases = [ ("dcthoyueorza", "zydeco") 
                   , ("uruqrnytrois", "squinty")
                   , ("rryqeiaegicgeo??", "reacquiring")
                   , ("udosjanyuiuebr??", "jaybirds")
                   , ("vaakojeaietg????????", "straightjacketed") ]

runTestCase ( (l, r), o ) = it (show (l, r)) (scrabble l r `shouldBe` o)

runTestCaseWith f (l, r) = it (show (l, r)) $ do
  dict_ <- getDict
  case dict_ of
    Left err -> fail (show err)
    Right dict -> runReaderT (f l) dict `shouldReturn` r

bonus_2_3 = mapM_ (runTestCaseWith longest) bonus2_testcases >>
  mapM_ (runTestCaseWith highest) bonus3_testcases

main = (hspec $ mapM_ runTestCase (example_testcases ++ bonus1_testcases)) >> (hspec bonus_2_3)
