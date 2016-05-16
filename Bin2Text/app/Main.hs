{-# LANGUAGE BangPatterns #-}
module Main where


import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Ord
import           Data.List
import           Data.Int
import           Data.Either
import           Options.Applicative
import           Control.Monad.Except
import           ADT

data Input = Input FilePath FilePath Int Int Double Double deriving Show

highestN :: Int -> [Item] -> [Item]
highestN n = take n . sortBy (flip (comparing snd))

lowestN :: Int -> [Item] -> [Item]
lowestN n = take n . sortBy (comparing snd)

out3 :: Int32 -> [Item] -> [Item]
out3 k = foldr acc []
  where acc t@(!ts, !avg) xs
            | k `elem` ts = insertBy (comparing snd) t xs
            | k `notElem` ts = xs

cmdline :: Parser Config
cmdline = Config
    <$> option auto (short 'n' <> metavar "INT" <> help "number of sorted tuples for out1.txt and out2.txt" )
    <*> option auto (short 'k' <> metavar "INT" <> help "number of sorted tuples for out3.txt" )
    <*> option auto (short 'b' <> metavar "FLOAT" <> help "bin width for out4.txt" )
    <*> optional (option auto (short 's' <> metavar "FLOAT" <> help "number of standard deviations for out5.txt" ))
    <*> optional (option auto (short 't' <> metavar "INT" <> help "number of threads"  ))
    <*> argument str (metavar "input1" <> metavar "INPUT1" <> help "input file 1" )
    <*> argument str (metavar "input2" <> metavar "INPUT2" <> help "input file 2" )

greet :: Config -> IO ()
greet config = do
    st <- runExceptT (fromConfig config)
    case st of
        Left e -> print e
        Right sts -> run sts app >>= print --print . fmap (take 10 . reverse . __tuples_)

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> cmdline)
             ( fullDesc
          <> progDesc "Convert predefined binary file to text file"
          <> header "bin2text " )
