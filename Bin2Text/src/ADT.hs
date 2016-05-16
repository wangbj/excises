{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ADT
    (
      Header(..)
    , ContentData(..)
    , Config(..)
    , AppState(..)
    , fromConfig
    , App(..)
    , run
    , app
    , headerFromFile
    , saveHeader
    , fromContent
    , saveContent
    , encodeHeader
    , decodeHeader      
    , encodeContent
    , decodeContent
    , contentFromFiles
    , Item
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict (IntMap)
import           Data.Serialize
import           Data.Either
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import           Control.Concurrent.Async
import qualified Data.Conduit.Cereal as Conduit
import qualified Data.Conduit.Combinators as Conduit
import           Data.Conduit
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V
import           Data.Vector.Mutable (IOVector)
import           Data.Int
import           GHC.Generics
import           System.IO
import           Text.Printf
import           Data.Maybe
import           Data.Monoid
import qualified Data.Builder as DB

import           Tuple
import           FMT

data Header = Header {
    __zero       :: Int32
  , __dimensions :: Int32
  , __numVars    :: Int32
  , __numTuples  :: Int64
  , __avgScores  :: Double
  } deriving Eq

instance Show Header where
  show (Header _ dim numVars numTuples avgScores) =
    "Header " ++ show dim ++ " " ++ show numVars ++ " " ++ show numTuples
    ++ " " ++ show avgScores

instance Serialize Header where
    put = putHeader
    get = getHeader

type Item = ([Int32], Double)

fromItem (!ts, !avg) = Tuple ts avg
{-# INLINE fromItem #-}

data ContentData = ContentData {
    __dimensions_ :: !Int
  , __numVars_    :: !Int
  , __numTuples_  :: !Int
  , __avgScores_  :: !Double
  , __tuples_     :: [Item]
  } deriving (Generic, Eq, Show)

instance Serialize ContentData where

headerFromFile :: FilePath -> ExceptT String IO Header
headerFromFile fname = liftIO (B.readFile fname) >>= ExceptT . return . decodeHeader

putItem :: Item -> Put
putItem (ts, avg) = mapM_ (putWord32le . fromIntegral) ts >> putFloat64le avg

getHeader :: Get Header
getHeader = Header <$> getInt32le <*> getInt32le <*> getInt32le <*> getInt64le <*> getFloat64le

putHeader :: Header -> Put
putHeader (Header _ dim numVars numTuples avg) = 
       putWord32le 0
    >> putWord32le (fromIntegral dim)
    >> putWord32le (fromIntegral numVars)
    >> putWord64le (fromIntegral numTuples)
    >> putFloat64le avg

encodeHeader :: Header -> ByteString
encodeHeader = runPut . putHeader

decodeHeader = runGet getHeader

encodeContent :: ContentData -> ByteString
encodeContent (ContentData _ _ _ _ tuples) = runPut (mapM_ putItem tuples)

saveHeader :: Header -> FilePath -> IO ()
saveHeader header filename = B.writeFile filename . encodeHeader $ header

fromContent :: ContentData -> Header
fromContent ctx@(ContentData dim numVars numTuples avg tuples) =
    Header 0 (fromIntegral dim) (fromIntegral numVars) (fromIntegral numTuples) avg

saveContent :: ContentData -> FilePath -> FilePath -> IO ()
saveContent ctx@(ContentData dim numVars numTuples avg tuples) headerfile contentfile =
    saveHeader header headerfile >> B.writeFile contentfile (encodeContent ctx)
    where header = Header 0 (fromIntegral dim) (fromIntegral numVars) (fromIntegral numTuples) avg

getT :: Int -> Get [Int32]
getT n = replicateM n getInt32le
{-# INLINE getT #-}

getItem :: Int -> Get Item
getItem d = do
    xs <- getT (fromIntegral d) :: Get [Int32]
    avg <- getFloat64le
    return (xs, avg)

getAllItems_ n d = replicateM (fromIntegral n) (getItem d)

getAllItems n d = runGet (getAllItems_ n d)

decodeContent :: Header -> ByteString -> Either String ContentData
decodeContent header@(Header _  dim numVars numTuples avgScores) bs
  | isLeft decoded     = Left $! "decode error, cannot read any tuples" ++ show header
  | numDecodedTuples /= (fromIntegral numTuples) = Left $! "expecting " ++ show numTuples ++ ", read " ++ show numDecodedTuples
  | numDecodedTuples == (fromIntegral numTuples) = Right $! ContentData (fromIntegral dim) (fromIntegral numVars) (fromIntegral numTuples) avgScores tuples
  where decoded = getAllItems numTuples (fromIntegral dim) bs
        (Right tuples) = decoded
        numDecodedTuples = length tuples

contentFromFiles :: FilePath -> FilePath -> ExceptT String IO ContentData
contentFromFiles headerfile contentfile = do
    header <- headerFromFile headerfile
    contents <- lift $ B.readFile contentfile
    ExceptT . return $ decodeContent header contents

data Config = Config {
      highestLowest :: Int
    , highestLowestContains :: Int
    , histogram :: Double
    , standardDeviations :: Maybe Double
    , numThreads :: Maybe Int
    , headerFile :: FilePath
    , contentFile :: FilePath
    } deriving Show

data AppState = AppState {
    appConfig   :: Config
  , appHeader   :: Header
    }

fromConfig config =  (liftIO (openFile (contentFile config) ReadMode )) >>= \h ->
    headerFromFile (headerFile config) >>= \hdr ->    
    return $! AppState { appConfig   = config, appHeader = hdr
                       }
    where dim             = fromIntegral . __dimensions
          numTuples       = fromIntegral . __numTuples

newtype App a = App {
    runApp :: ReaderT AppState (ExceptT String IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState, MonadThrow)

run :: AppState -> App a -> IO (Either String a)
run config app = bracket ( return () )
    (\_ -> return () )
    (\_ -> runExceptT (runReaderT (runApp app) config))

tuplesConsumer :: Consumer Tuple App ()
tuplesConsumer = Conduit.mapM_ (liftIO . print)

appDataEngine = do
    numTuples <- asks (__numTuples . appHeader)
    dim       <- asks (fromIntegral . __dimensions . appHeader)
    avg       <- asks (__avgScores . appHeader)        
    optSM     <- asks (standardDeviations . appConfig)
    hist      <- asks (histogram . appConfig)
    optN      <- asks (highestLowest . appConfig)
    (sumsq, (TopSorted _ _ top0, BotSorted _ _ bot0)) <- runConduit $ itemProducer =$= itemConsumer
    let top      = reverse top0
        bot      = reverse bot0
        minT     = head bot
        maxT     = head top
        minScore = tupleScore minT
        maxScore = tupleScore maxT
        std     = sqrt (sumsq / fromIntegral numTuples)
        nHist    = 1 + floor ((maxScore - minScore) / hist) :: Int
        bkt scr  = floor ((scr - minScore) / hist) :: Int
    liftIO $ print (minT, maxT, nHist)
    when (optN > 0) $ (outputT "out1.txt" top >> outputT "out2.txt" bot)
    liftIO $ writeFile "out4.txt" ""
    out4hdl <- liftIO $ openFile "out4.txt" WriteMode
    liftIO $ hPutStrLn out4hdl $ printf "%.10f\t%.10f" minScore maxScore
    case optSM of
        Nothing -> return ()
        Just _  -> liftIO $ writeFile "out5.txt" ""
    runConduit $ itemProducer =$= consume45 dim avg std out4hdl optSM nHist bkt
    liftIO $ hClose out4hdl
    return (minT, maxT, avg, std)

consume45 dim avg std hdl optS nHist bkt = getZipConduit
    $  ZipConduit (consume4 hdl nHist bkt)
    *> ZipConduit (consume5 dim avg std optS)

writeOut4 :: Handle -> IntMap Int -> IO ()
writeOut4 hdl = hPutBuilder hdl . IntMap.foldl acc mempty
  where
    acc w v = w <> intDec v <> char7 '\n'

consume4 hdl nHist g = Conduit.foldl acc im >>= liftIO . writeOut4 hdl
  where
    acc m (_, (_, scr)) = IntMap.insertWith (+) (g scr) 1 m
    im                  = IntMap.fromList (zip [1..nHist] (repeat 0)) :: IntMap Int

blk :: Int -> Double -> Double -> Double -> ConduitM (Int, Item) Builder App ()
blk dim avg s std = yield =<< go 1 mempty
  where
    blksize = 1024
    go !k r | k  > blksize = return r
            | k <= blksize = do
                  xm <- await
                  case xm of
                      Nothing -> return r 
                      Just (_, (ts, scr))  -> do
                          if cutoffPred dim avg s std scr then do
                              let !diff = (scr - avg) / std
                              b <- liftIO (buildOut5_ diff ts scr)
                              go (1+k) (r <> b)
                              else go k r

consume5 _ _ _ Nothing = return ()
consume5 dim avg std (Just optS) = Conduit.peekForever (blk dim avg optS std) =$= sink5
sink5 = do
    hdl <- liftIO $ openFile "out5.txt" WriteMode
    Conduit.mapM_ (liftIO . hPutBuilder hdl)
    liftIO (hClose hdl)

cutoffPred dim avg s std score
    | even dim && score >= avg + (s*std) = True
    | odd dim  && score <= avg - (s*std) = True
    | otherwise = False

outputT :: FilePath -> [Tuple] -> App ()
outputT p t = (liftIO (B.writeFile p B.empty)) >> (runConduit $ src =$= snk)
   where src = Conduit.yieldMany (init t) :: Producer App Tuple
         snk = Conduit.mapM_ (liftIO . (appendFile p) . pprint)
         pprint (Tuple ts d) = (concatMap (\i -> show i ++ "\t") ts) ++ (printf "%.10f\n" d)

app = appDataEngine
    
itemProducer :: Producer App (Int, Item)
itemProducer = do
    st          <- ask
    fp          <- asks (contentFile . appConfig)
    hndl        <- liftIO $ openFile fp ReadMode
    let prod  = Conduit.sourceHandle hndl
        !dim  = fromIntegral . __dimensions . appHeader $ st
        !nt   = fromIntegral . __numTuples  . appHeader $ st :: Int
        p1    = prod $= Conduit.conduitGet2 (getItem dim)
    toProducer . getZipSource $ (,)
        <$> ZipSource (Conduit.enumFromTo 0 (nt-1))
        <*> ZipSource p1

saveItem :: Consumer (Int, Item) App ()
saveItem = return ()
{-
saveItem = asks appVect >>= \vect ->
    Conduit.mapM_ ((\(!k, !item) -> liftIO (MV.write vect k (fromItem item))))
-}

type Sorted = (TopSortedTuples, BotSortedTuples)

output12 :: Consumer (Int, Item) App (Double, Sorted)
output12 = do
    optN <- asks (highestLowest . appConfig)
    avg <- asks (__avgScores . appHeader)    
    Conduit.foldl (step avg) (0, (emptyTopSorted (1+optN), emptyBotSorted (1+optN)))
  where step avg (!s, !(!top, !bot)) (!_, !item@(_, !score)) = (s + d*d, ( insertTop (fromItem item) top
                                                                         , insertBot (fromItem item) bot ) )
          where !d = score - avg

data K3 = K3 {-# UNPACK #-} !Int {-# UNPACK #-} !Double !TopSortedTuples !BotSortedTuples deriving Show

type M3 = IntMap K3
insK :: Int -> Item -> M3 -> M3
insK optK item@(ts, scr) = go ts
  where go []     m = m
        go (x:xs) m = go xs m'
            where
              (K3 cnt sums top bot) = maybe (K3 0 0 (emptyTopSorted optK) (emptyBotSorted optK)) id (IntMap.lookup (fromIntegral x) m)
              top' = insertTop (fromItem item) top
              bot' = insertBot (fromItem item) bot
              m'   = IntMap.insert (fromIntegral x) (K3 (1+cnt) (scr+sums) top' bot') m

conduit3 :: Int -> Int -> ConduitM (Int, Item) (Int, K3) App ()
conduit3 nv k = Conduit.foldl step im >>= Conduit.yieldMany . IntMap.assocs
  where im = IntMap.empty
        step w (_, item) = insK k item w

conduit3_ = do
    numVars <- asks (fromIntegral . __numVars . appHeader)
    optK    <- asks (highestLowestContains . appConfig)
    conduit3 numVars optK

showTupleAcc m (Tuple ts scr) = foldl acc m ts <> string7 p10
  where p10 = printf "%.10f\n" scr
        acc w i = w <> int32Dec i <> char7 '\t'

showTuple = showTupleAcc mempty

showTop :: TopSortedTuples -> Builder
showTop (TopSorted _ _ ts) = foldl showTupleAcc mempty (reverse ts)

showBot :: BotSortedTuples -> Builder
showBot (BotSorted _ _ ts) = foldl showTupleAcc mempty (reverse ts)

buildk3 :: (Int, K3) -> Builder
buildk3 (!v, !(K3 cnt sums top bot)) = intDec v <> string7 scr <> char7 '\n' <> showTop top <> showBot bot
    where scr = printf "\t%.10f" (sums / fromIntegral cnt)

consume3 :: Consumer (Int, K3) App ()
consume3 = do
    hdl <- liftIO $ openFile "out3.txt" WriteMode
    Conduit.mapM_ (liftIO . hPutBuilder hdl . buildk3)
    liftIO (hClose hdl)

output3 = conduit3_ =$= consume3

itemConsumer :: Consumer (Int, Item) App (Double, Sorted)
itemConsumer = getZipConduit $ ZipConduit output3 *> ZipConduit output12
