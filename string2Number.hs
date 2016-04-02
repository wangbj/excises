{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Char8 as C
import System.IO
import System.Environment

dict = [   ("million", 10^6)
         , ("thousand", 10^3)
         , ("hundred", 10^2)
         , ("ninety", 90)
         , ("eighty", 80)
         , ("seventy", 70)
         , ("sixty", 60)
         , ("fifty", 50)
         , ("forty", 40)
         , ("thirty", 30)
         , ("twenty", 20)
         , ("nineteen", 19)
         , ("eighteen", 18)
         , ("seventeen", 17)
         , ("sixteen", 16)
         , ("fifteen", 15)
         , ("fourteen", 14)
         , ("thirteen", 13)
         , ("twelve", 12)
         , ("eleven", 11)
         , ("ten", 10)
         , ("nine", 9)
         , ("eight", 8)
         , ("seven", 7)
         , ("six", 6)
         , ("five", 5)
         , ("four", 4)
         , ("three", 3)
         , ("two", 2)
         , ("one", 1)
         , ("zero", 0)
         ] :: [ (C.ByteString, Int) ]

negative = "negative" :: C.ByteString

isTimes x
  | x == 10^2 || x == 10^3 || x == 10^6 = True
  | otherwise = False

notTimes = not . isTimes

accum r d
  | d == 10^2 = (r`div`1000)*1000 + (r `mod` 1000)*d
  | d == 10^3 = (r`div`10^6)*10^6 + (r `mod` 10^6)*d
  | d == 10^6 = r*d

parseNumber :: C.ByteString -> Maybe Int
parseNumber (C.words -> []) = Nothing
parseNumber (C.words -> (w:ws)) 
  | w == negative = fmap (\x -> -x) (go 0 ws)
  | w /= negative = go 0 (w:ws)
  where go r [] = Just r
        go r (x:xs) = lookup x dict >>= \d -> if
          | isTimes d -> go (accum r d) xs
          | notTimes d -> go (r + d) xs

parseNumber' = maybe 0 id . parseNumber

main = fmap head getArgs >>= \fname ->
  withFile fname ReadMode mainloop
  where mainloop h = C.hGetContents h >>= mapM_ (print . parseNumber') . C.lines
