{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Unsafe #-}

module FMT (
    buildOut5_    
  , buildOut5
    ) where

import qualified Data.ByteString as B
import           Data.ByteString.Unsafe
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Monoid

import           Tuple

import           Foreign
import           Foreign.C
import           Foreign.C.String

import           System.IO.Unsafe

foreign import ccall safe "d10p" c_d10p :: Double -> IO CString
foreign import ccall safe "d10p2" c_d10p2 :: Double -> Double -> IO CString

dbl :: Double -> Double -> IO ByteString
dbl x y = c_d10p2 x y >>= B.packCString
{-# INLINE dbl #-}

buildOut5_ :: Double -> [Int32] -> Double -> IO Builder
buildOut5_ diff ts avg = do
    let !p1  = foldl acc mempty ts
    p2 <- dbl avg diff
    return $! p1 <> byteString p2
  where
      acc w x = w <> (int32Dec x) <> char7 '\t'
      {-# INLINE acc #-}

buildOut5 d t v = unsafePerformIO (buildOut5_ d t v)
