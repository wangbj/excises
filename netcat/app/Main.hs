module Main where

import Network.Simple.TCP
import Options.Applicative
import Data.Monoid
import Netcat

data Config = Config {
    isServer :: Bool
  , hostName :: String
  , portNum  :: Int
  } deriving Show


cmdline :: Parser Config
cmdline = Config
  <$> switch (long "listen" <> short 'l' <> help "listening on port")
  <*> argument str (metavar "hostname" <> help "hostname")
  <*> argument auto (metavar "portNumber" <> help "port number")

greet :: Config -> IO ()
greet (Config listen hostname port)
  | listen     = netcatd port
  | not listen = netcat hostname port

main :: IO ()
main = withSocketsDo $ execParser opts >>= greet
  where
    opts = info (helper <*> cmdline)
      (    fullDesc
        <> progDesc "netcat serve or connect to <hostname> <port>"
        <> header "netcat" )
