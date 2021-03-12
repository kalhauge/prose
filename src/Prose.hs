{-# LANGUAGE ImportQualifiedPost #-}
module Prose where

import System.IO (stdout, stderr, stdin)

import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Text.Megaparsec

import Prose.Text.Parser
import Prose.Text.Serializer

app :: IO ()
app = do
  txt <- Text.hGetContents stdin
  case runSimpleParser pSimpleDoc txt of
    Left e ->
      Text.hPutStr stderr (Text.pack $ errorBundlePretty e)
    Right doc ->
      Text.hPutStr stdout (runSimpleSerializer sDoc doc)
