{-|
Module      : Crypto
Description : Hash functions
Copyright   : (c) Gautham Ganapathy, 2018
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Hash functions

-}

module Crypto
  ( hashSHA1
  , UUID
  , makeUUID) where

import Foundation hiding (hash)
import Data.ByteString (ByteString)
import Crypto.Hash (Digest, SHA1, hash)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)

type UUID = ByteString

hashSHA1 :: ByteString -> ByteString
hashSHA1 = convert . (hash :: ByteString -> Digest SHA1)

makeUUID :: IO (UUID)
makeUUID = hashSHA1 <$> getRandomBytes 64 

