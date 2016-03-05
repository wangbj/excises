{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as C
import Data.Maybe
import System.Environment
import System.IO
import Control.Exception
import Control.Monad.State
import Data.Monoid

size8K = 8192
read8KBackward :: Integer -> Handle -> IO (Maybe (Int, C.ByteString, Integer))
read8KBackward off handle = do
  if off <= 0 then return Nothing else do
    let off' = max 0 (off - size8K)
        size = fromIntegral (off - off')
    buff <- liftIO $ hSeek handle AbsoluteSeek off' >> C.hGetSome handle size
    return . Just $ (size, buff, off')

data Progress = Done Integer   -- Finished
              | More Int       -- Need mores lines to consume

cnt n s size off handle = case go 0 0 (size-1) of
  Done x -> return x
  More n' -> do
    r1 <- read8KBackward off handle
    case r1 of
      Nothing -> return off
      Just (bytes, bs, off') -> cnt n' bs bytes off' handle
  where go r i k
          | r >= n = Done (off + fromIntegral (size - i + 1))
          | k < 0 = More (n-r)
          | s `C.index` k == '\n' = if k > 0 && s `C.index` (k-1) == '\r' then go (succ r) (i+2) (k-2) else go (succ r) (succ i) (pred k)
          | s `C.index` k == '\r' = if k > 0 && s `C.index` (k-1) == '\n' then go (succ r) (i+2) (k-2) else go (succ r) (succ i) (pred k)
          | otherwise = go r (succ i) (pred k)
          
doTail 0 handle = return ()
doTail n handle = do
  fileSize <- hFileSize handle
  if fileSize <= 0 then return () else do
    Just (size, bs, off') <- read8KBackward fileSize handle
    pos <- if (C.last bs) `elem` "\r\n" then cnt (1+n) bs size off' handle
           else cnt n bs size off' handle
    hSeek handle AbsoluteSeek pos
    lines <- C.hGetContents handle
    C.putStr lines

tailFile :: Int -> FilePath -> IO ()
tailFile n path = withFile path ReadMode (doTail n)

main :: IO ()
main = do
  (opt:args) <- getArgs
  let (n, flist) = case C.readInt (C.pack opt) of
        Nothing -> (10, opt:args)
        Just (!x,_) -> (-x, args)
  mapM_ (tailFile n) flist
