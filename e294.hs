{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client (newManager)
import           Servant.API
import           Servant.Client
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Data.Char
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Function
import           Data.Ord
import           Control.Monad.Reader
import           Control.Monad
import           Test.Hspec
import           Test.Hspec.Core.Spec

type API = Get '[OctetStream] ByteString

api :: Proxy API
api = Proxy

getDict :: IO (Either ServantError [Text])
getDict =  do
  manager <- newManager tlsManagerSettings
  res <- runClientM (client api) (ClientEnv manager (BaseUrl Https "storage.googleapis.com" 443 "/google-code-archive-downloads/v2/code.google.com/dotnetperls-controls/enable1.txt"))
  return $ Text.splitOn "\r\n" . Text.decodeUtf8 <$> res

fromList :: Text -> IntMap Int
fromList = IntMap.fromListWith (+) . flip zip (repeat 1) . xlate
  where
    xlate :: Text -> [Int]    -- ^ treat '?' as a special char.
    xlate = map (\x -> if x == '?' then maxBound else ord x) . Text.unpack

scrabbleHelper :: IntMap Int -> IntMap Int -> Bool
scrabbleHelper tiles word
  | IntMap.null word  = True
  | IntMap.null tiles = False
scrabbleHelper t@(IntMap.minViewWithKey -> Just ((k, v), m)) w@(IntMap.minViewWithKey -> Just ((k', v'), m'))
  | k == maxBound = scrabbleHelper (IntMap.update (\_ -> if v <= v' then Nothing else Just (v - v')) k t)
                    (IntMap.update (\_ -> if v < v' then Just (v' - v) else Nothing) k' w)
  | k /= k'       = scrabbleHelper m w                    
  | v >= v'       = scrabbleHelper m m'
  | v  < v'       = scrabbleHelper m (IntMap.insert k' (v' - v) m')

scrabble :: Text -> Text -> Bool
scrabble = on scrabbleHelper fromList

matchLongest :: Text -> Text -> Text -> Text
matchLongest tile res word 
  | scrabble tile word == False = res
  | Text.length word  > Text.length res = word
  | Text.length word <= Text.length res = res

longest :: Text -> ReaderT [Text] IO Text
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
getPoints tile = Text.foldl acc 0
  where
    acc pts c = pts + fromMaybe 0 (Text.find (== c) tile >>= flip lookup points)

matchHighest :: Text -> (Int, Text) -> Text -> (Int, Text)
matchHighest tile res@(p, t) word
  | scrabble tile word == False = res
  | pts                 > p     = (pts, word)
  | pts                <= p     = res
  where pts = getPoints tile word

highest :: Text -> ReaderT [Text] IO Text
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

runTestCases ( (l, r), o ) = it (show (l, r)) (scrabble l r `shouldBe` o)

{-
runTestCases' (l, r) = it (show (l, r)) $ do
  dict_ <- runIO getDict
  case dict_ of
    Left err -> fail (show err)
    Right dict -> do
      x <- runIO $ runReaderT (longest l) dict
      return $! x `shouldBe` r
-}

main = hspec $ mapM_ runTestCases (example_testcases ++ bonus1_testcases)
