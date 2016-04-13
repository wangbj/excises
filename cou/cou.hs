import qualified Data.ByteString.Char8 as C
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Foreign.C

foreign import ccall "ccou" ccou :: CString -> CInt -> IO CInt

bfh :: C.ByteString -> Int -> Int -> Int
bfh u i s
  | i >= (C.length u) = 0
  | u `C.index` i == 'P' = 1 + bfh u (succ i) s
  | u `C.index` i == 'N' = (-1) + max (bfh u (succ i) s)
                                      (bfh u (i+s+1) (2*s))

bf :: C.ByteString -> Int
bf s = bfh s 0 1

newtype Input = Input String deriving Show

instance Arbitrary Input where
  arbitrary = fmap Input (resize 64 (listOf1 (elements "NP")))

prop1 :: Input -> Property
prop1 (Input s) = monadicIO $ do
  let len = fromIntegral (length s)  
  ccr <- run (newCString s >>= \cs -> fmap fromIntegral (ccou cs len))
  let cr = bf (C.pack s)
  assert (cr == ccr)

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 100000 } prop1
