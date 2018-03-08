{-|
Module      : MetaInfo
Description : Torrent metainfo
Copyright   : (c) Gautham Ganapathy, 2018
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Module for managing torrent metainfo as well as serializing/deserializing..

-}

module Data.MetaInfo
  ( FileProp(..)
  , FileInfo(..)
  , Info(..)
  , MetaInfo(..)
  , decode ) where

import Foundation hiding (fromInteger)
import Foundation.VFS

import Prelude (fromInteger)

import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Bencoding as Benc
import Crypto (hashSHA1)

type ErrorMsg = Text

type MD5Sum = ByteString
type Size = Int64

-- | File properties
data FileProp = FileProp { filePropLength :: Size            -- ^ File size in bytes
                         , filePropMD5Sum :: Maybe MD5Sum    -- ^ MD5 checksum for the file
                         , filePropPath   :: Maybe FilePath  -- ^ Path to the file
                         } deriving (Show)

-- | Single-/Multi-file information
data FileInfo = SingleFileInfo { singleFileName :: Text        -- ^ File name
                               , singleFileProp :: FileProp    -- ^ File properties
                               }                               -- ^ Information for single file torrent
              | MultiFileInfo { multiFileDirName :: FilePath   -- ^ Directory name
                              , multiFileProps   :: [FileProp] -- ^ Properties of files
                              }                                -- ^ Information for multi file torrent
                deriving (Show) 

-- | Common file info
data Info = Info { miPieceLength :: Int64         -- ^ Piece size in bytes
                 , miPieces      :: [ByteString]  -- ^ List of SHA hashes of pieces in the torrent
                 , miPrivate     :: Bool          -- ^ Private torrent?
                 , miFileInfo    :: FileInfo      -- ^ Single/multi-file info
                 } deriving (Show)
                                 
-- | Torrent Metainfo                
data MetaInfo = MetaInfo { miInfo         :: Info            -- ^ Torrent info
                         , miInfoHash     :: ByteString      -- ^ Hash of the info dict
                         , miAnnounce     :: Text            -- ^ Announce URL
                         , miAnnounceList :: Maybe [[Text]]  -- ^ Alternative announce URL list
                         , miCreationDate :: Maybe Text      -- ^ Creation date
                         , miComment      :: Maybe Text      -- ^ Comment
                         , miCreatedBy    :: Maybe Text      -- ^ Creator
                         , miEncoding     :: Maybe Text      -- ^ Encoding
                         } deriving (Show)

parseFileProp :: Benc.BencDict -> Either ErrorMsg FileProp
parseFileProp d = do
  len <- Benc.lookupInt "length" d
  let md5 = Benc.lookupStrOpt "" "md5sum" d
  return $ FileProp len (if md5 == "" then Nothing else Just md5) Nothing

parseSingleFileInfo :: Benc.BencDict -> Either ErrorMsg FileInfo
parseSingleFileInfo infoDict = do
  fileName <- decodeUtf8 <$> Benc.lookupStr "name" infoDict
  fileProp <- parseFileProp infoDict
  return $ SingleFileInfo fileName fileProp

parseMultiFileInfo :: Benc.BencDict -> Either ErrorMsg FileInfo
parseMultiFileInfo _ = Left "Not implemented"

splitByteString :: Int -> ByteString -> [ByteString]
splitByteString n bs
  | BS.length bs == 0 = []
  | otherwise         = let (prefix, suffix) = BS.splitAt n bs
                     in prefix:(splitByteString n suffix)
                        
parseInfo :: Benc.BencDict -> Either ErrorMsg Info
parseInfo infoDict = do
  pieceLen <- Benc.lookupInt "piece length" infoDict
  pcs <- splitByteString 20 <$> Benc.lookupStr "pieces" infoDict
  let priv = Benc.lookupIntOpt 0 "private" infoDict
  fi <- case M.lookup "files" infoDict of
          Just _  -> parseMultiFileInfo infoDict
          Nothing -> parseSingleFileInfo infoDict

  return $ Info pieceLen pcs (priv /= 0) fi

parseAnnounceList :: Benc.BencElement -> [[Text]]
parseAnnounceList (Benc.BencList l) = fmap parse' l
  where parse' (Benc.BencList l') = fmap parse'' l'
        parse' _ = []
        parse'' (Benc.BencString s) = decodeUtf8 s
        parse'' _ = ""
parseAnnounceList _ = []

strTime :: Int64 -> Text
strTime = pack . formatTime defaultTimeLocale "%c" . posixSecondsToUTCTime . fromInteger . toInteger

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
  
parseMetaInfo :: Benc.BencDict -> Either ErrorMsg MetaInfo
parseMetaInfo metaInfoDict = do
  infoDict <- Benc.lookupDict "info" metaInfoDict
  info <- parseInfo infoDict
  annce <- decodeUtf8 <$> Benc.lookupStr "announce" metaInfoDict
  let annceList = case M.lookup "announce-list" metaInfoDict of
                       Just v  -> parseAnnounceList v
                       Nothing -> []
      crDate = eitherToMaybe $ strTime <$> Benc.lookupInt "creation date" metaInfoDict
      cmmnt = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "comment" metaInfoDict
      crBy = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "created by" metaInfoDict
      enc = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "encoding" metaInfoDict
      infoHash = (hashSHA1 . Benc.encode) $ Benc.BencDict infoDict
  return $ MetaInfo info infoHash annce (Just annceList) crDate cmmnt crBy enc

-- | Decodes BitTorrent metainfo from a Bencoded dictionary element.
decode :: Benc.BencElement -> Either ErrorMsg MetaInfo
decode (Benc.BencDict metaInfoDict) = parseMetaInfo metaInfoDict
decode _                            = Left "meta-info dictionary expected"
