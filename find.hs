{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Find (
    FileType(..), FileInfo(..)
  , find
  , findIf
    ) where

import qualified Data.Text as Text
import Data.Text(Text)
import System.Directory
import qualified System.Posix.Files as Files
import Control.Exception
import Control.Monad
import Control.Monad.Except
import System.IO
import Data.Time
import Data.Time.Clock.POSIX
import Data.Bits
import System.FilePath

data FileType = BlockDevice
              | CharDevice
              | NamedPipe
              | RegularFile
              | Directory
              | SymLink
              | Socket
              deriving (Show, Eq)
                       
data FileInfo = FileInfo {
    fileName  :: Text
  , fileSize  :: Integer
  , fileType  :: FileType
  , fileModes :: Int
  , fileOwner :: Int
  , fileGroup :: Int
  , fileMtime :: UTCTime
  , fileAtime :: UTCTime
  , fileCtime :: UTCTime
  } deriving Show

fromTypeModes t 
  | t == Files.blockSpecialMode     = BlockDevice
  | t == Files.characterSpecialMode = CharDevice
  | t == Files.namedPipeMode        = NamedPipe
  | t == Files.regularFileMode      = RegularFile
  | t == Files.directoryMode        = Directory
  | t == Files.symbolicLinkMode     = SymLink
  | t == Files.socketMode           = Socket

fromPosixTime :: POSIXTime -> UTCTime
fromPosixTime = posixSecondsToUTCTime

fromFileStatus fname st = FileInfo {
    fileSize  = fromIntegral (Files.fileSize st)
  , fileName  = Text.pack fname
  , fileModes = fromIntegral mode
  , fileType  = ft
  , fileOwner = fromIntegral (Files.fileOwner st)
  , fileGroup = fromIntegral (Files.fileGroup st)
  , fileMtime = fromPosixTime (Files.modificationTimeHiRes st)
  , fileAtime = fromPosixTime (Files.accessTimeHiRes st)
  , fileCtime = fromPosixTime (Files.statusChangeTimeHiRes st)                
  }
  where mode = (Files.fileMode st) .&. (fromIntegral Files.accessModes)
        ft   = fromTypeModes (
                   (fromIntegral (Files.fileMode st)) .&. (fromIntegral Files.fileTypeModes))

getFileStatus :: FilePath -> IO (Either IOException FileInfo)
getFileStatus fname = do
  st <- try (Files.getFileStatus fname)
  case st of
    Left e -> return $! Left e
    Right st' -> return $! Right (fromFileStatus fname st')

getFileStatus' :: FilePath -> ExceptT IOException IO FileInfo
getFileStatus' fname = do
  res <- liftIO $ try (Files.getFileStatus fname)
  case res of
    Left e -> throwError e
    Right st -> return (fromFileStatus fname st)

isDirectory info = fileType info == Directory

concatMapM g f = mapM g f >>= return . concat

satisfyAllPreds [] t = True
satisfyAllPreds (p:ps) t
  | p t == True  = satisfyAllPreds ps t
  | p t == False = False
{-# INLINE satisfyAllPreds #-}

walkIfs preds path = getFileStatus' path >>= \st -> let t = fileType st in if
  | t == Directory -> directoryContents' path >>= \subdir ->
                      concatMapM (walkIfs preds) (fmap (path </>) subdir) >>= \res ->
                      if (satisfyAllPreds preds st) then return (st:res) else return res
  | t `elem` [SymLink , CharDevice , BlockDevice , NamedPipe , Socket , RegularFile ] -> if (satisfyAllPreds preds st) then return [st] else return []
  where directoryContents' = liftIO . fmap (drop 2) . getDirectoryContents

walkIf pred = walkIfs [pred]

walk = walkIf (const True)

findIf preds = walkIfs preds
find         = walk
