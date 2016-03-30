import qualified Data.ByteString.Char8 as C
import Data.Array.Unboxed
import System.Environment
import System.IO

getinputs = map getline . C.lines
  where getline s = (s1, s2)
          where (s1:s2:_) = C.split ';' s

data P = P {-# UNPACK #-} !Int String

instance Eq P where
  (P x _) == (P y _) = x == y

instance Ord P where
  compare (P x _) (P y _) = compare x y

instance Show P where
  show (P _ s) = reverse s

lcs s t = cache ! (len1, len2)
  where u1   = listArray (1, len1) (C.unpack s) :: UArray Int Char
        u2   = listArray (1, len2) (C.unpack t) :: UArray Int Char
        len1 = C.length s
        len2 = C.length t
        go i j
          | i == 0 || j == 0 = P 0 []
          | u1 ! i == u2 ! j = append (u1!i) (cache ! (pred i, pred j))
          | u1 ! i /= u2 ! j = max (cache ! (pred i, j)) (cache ! (i, pred j))
        cache = listArray ((0,0), (len1, len2)) [ go x y | x <- [0..len1], y <- [0..len2]] :: Array (Int, Int) P
        append c (P x s) = P (succ x) (c:s)

main = do  fname:_ <- getArgs
           withFile fname ReadMode mainloop
       where mainloop handle = C.hGetContents handle >>= mapM_ (print . uncurry lcs) . getinputs
