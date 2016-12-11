module Import
  ( module Imports
  , utf8bs
  ) where

import ClassyPrelude as Imports

import qualified Data.Text.Encoding as TextEncoding

utf8bs :: Text -> ByteString
utf8bs = TextEncoding.encodeUtf8

