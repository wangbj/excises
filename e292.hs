{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec
import Text.Parsec
import Control.Monad.State

intS :: Monad m => ParsecT String s m String
intS = many1 digit

getNext k s  | x >  n2 = n1 * base + x
             | x <= n2 = (1 + n1) * base + x
    where    base = 10 ^ (length s)
             (n1, n2) = k `quotRem` base
             x = read s

dashed :: MonadState Int m => ParsecT String s m [Int]
dashed = do
  from <- liftM2 getNext get intS
  char '-'
  to <- intS
  let next = getNext from to
  put next
  return $! [from .. next]

coloned2 :: MonadState Int m => ParsecT String s m [Int]
coloned2 = do
  from <- liftM2 getNext get intS
  char ':'
  to <- intS
  char ':'
  step <- read <$> intS
  let next = getNext from to
  put next
  return $! [from, from + step .. next]

coloned1 :: MonadState Int m => ParsecT String s m [Int]
coloned1 = do
  from <- liftM2 getNext get intS
  char ','
  to <- intS
  char ':'
  step <- intS
  let from' = getNext from to
      next  = getNext from' step
  put next
  return $! from : [from'..next]

dotted :: MonadState Int m => ParsecT String s m [Int]
dotted = do
  from <- liftM2 getNext get intS
  string ".."
  to <- intS
  let next = getNext from to
  put next
  return [from .. next]
  
intP :: MonadState Int m => ParsecT String s m [Int]
intP = do
  next <- liftM2 getNext get intS
  put next
  return [next]

rangedP :: MonadState Int m => ParsecT String s m [Int]
rangedP = try coloned1
      <|> try coloned2
      <|> try dashed
      <|> try dotted
      <|> intP

parser :: MonadState Int m => ParsecT String s m [Int]
parser = concat <$> sepBy rangedP (char ',')

parseRangedInts :: String -> Either ParseError [Int]
parseRangedInts s = evalState (runParserT parser 0 "<stdin>" s) (-1)

testcases :: [ (String, Either ParseError [Int]) ]
testcases = [("1,3,7,2,4,1", pure [1,3,7,12,14,21]),("1-3,1-2", pure [1,2,3,11,12]),("1:5:2", pure [1,3,5]),("104-2", pure [104,105,106,107,108,109,110,111,112]),("104..02", pure [104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202]),("545,64:11", pure (545:[564..611]))]

main :: IO ()
main = hspec $ do
  describe "pass all initial test cases" $ do
    it "all success.." $ do
      and (map (\(i, o) -> parseRangedInts i == o) testcases) `shouldBe` True
