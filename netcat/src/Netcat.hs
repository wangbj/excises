module Netcat (
    netcatd
  , netcat
    ) where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception
import           Network.Simple.TCP

netcatdLoop :: Socket -> SockAddr -> IO Socket
netcatdLoop sk sa = recv sk blksiz >>= \bs_ -> case bs_ of
  Nothing -> return sk
  Just bs -> B.putStr bs >> netcatdLoop sk sa
  where blksiz = 8192

netcatLoop :: Socket -> SockAddr -> IO ()
netcatLoop sk sa = B.getLine >>= \bs -> unless (B.null bs) (send sk bs >> netcatLoop sk sa) 

--netcatd :: Int -> IO ()
--netcatd port = serve HostAny (show port) (uncurry netcatdLoop)

netcatd :: Int -> IO ()
netcatd port = bracket (listen HostAny (show port) prepareServer) closeSock (\_ -> return ())
  where prepareServer (server, _) = bracket (accept server (uncurry netcatdLoop)) closeSock (\_ -> return server)
                                                     
netcat :: String -> Int -> IO ()
netcat hostname port = connect hostname (show port) (uncurry netcatLoop)
