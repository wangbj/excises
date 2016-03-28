import Text.Parsec
import Data.Char
import Data.Monoid
import Data.List.Split
import Control.Applicative

hexChar :: (Monad m) => ParsecT String u m Char
hexChar = satisfy isHexDigit

hex2 :: (Monad m) => ParsecT String u m Int
hex2 = do
  x <- many1 hexChar
  return $! readHex x

hex2List :: (Monad m) => ParsecT String u m [Int]
hex2List = do
  x <- hex2
  return $! [x]

toHex c | c >= '0' && c <= '9' = ord c - ord '0'
        | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
        | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10

readHex = foldl acc 0
  where acc l c = 16*l + toHex c

colonOp :: (Monad m) => ParsecT String u m ([Int] -> [Int] -> [Int])
colonOp = do
  char ':'
  return $! (<>)

hexGroupMany1 :: (Monad m) => ParsecT String u m [Int]
hexGroupMany1 = hex2List `chainl1` colonOp

v6addr :: (Monad m) => ParsecT String u m [Int]
v6addr = option [] hexGroupMany1

parsev6 s | null s' = head . runParserT v6addr 0 "<stdin>" $ s1
          | (not . null) s' = liftA2 extend p1 p2
  where (s1:s') = splitOn "::" s
        (p1:p2:_) = concatMap (runParserT v6addr 0 "<stdin>") [s1, (head s')]
        extend x y = x ++ (replicate (8 - length x - length y) 0) ++ y

main = getContents >>= \s -> case parsev6 s of
  Left e -> putStrLn $ "parseError " ++ show e
  Right r -> print r
