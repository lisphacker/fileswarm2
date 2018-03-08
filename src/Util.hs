{-|
Module      : Util
Description : Utility functions
Copyright   : (c) Gautham Ganapathy, 2018
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Utility functions.
-}

module Util
  ( toStrictBS
  ) where

import Foundation
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Conversion (toByteString, ToByteString)

toStrictBS :: ToByteString a => a -> ByteString
toStrictBS = LBS.toStrict . toByteString
