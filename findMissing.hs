import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Monad

numDigits 0 = 0
numDigits x = 1 + numDigits ( x `div` 10)

readint = fst . fromJust . C.readInt
nexts st k s
  | len < digits = Nothing
  | len == digits = st `mplus` if readint s == 2+k then Just (1+k) else Nothing
  | x == 1 + k = nexts st x s'
  | x == 2 + k = case st of
      Nothing -> nexts (Just (1+k)) (2+k) s'
      Just _ -> Nothing
  | x /= (1+k) && x /= (2+k) = Nothing
  where digits = numDigits (1+k)
        len = C.length s
        (s1, s') = C.splitAt digits s
        x = readint s1

findMissing s = go 1
  where go k
          | k > (C.length s) `div` 2 = Nothing
          | otherwise = nexts Nothing (readint s1) s' `mplus` go (1+k)
            where (s1, s') = C.splitAt k s
