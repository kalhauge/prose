{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Prose where

-- base
import System.IO (stdout, stderr, stdin)
import Data.Foldable
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- optparse
import Options.Applicative

-- text
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-- bytestring
import Data.ByteString.Lazy qualified as BL

-- megaparsec
import Text.Megaparsec

-- aeson
import Data.Aeson

import Prose.Text.Parser qualified as P
import Prose.Text.Serializer qualified as S
import Prose.Pandoc

data Command
  = Format (Maybe FilePath)
  | Pandoc (Maybe FilePath)


parseArgs :: IO Command
parseArgs = execParser $ info parseCommand mempty
 where
  parseCommand :: Parser Command
  parseCommand = hsubparser $ fold
    [ command "format" $ info
      ( Format <$> optional
        ( strArgument ( metavar "FILE" )
        )
      )
      (progDesc "formats the file")
    , command "pandoc" $ info
      ( Pandoc <$> optional
        ( strArgument ( metavar "FILE" )
        )
      )
      (progDesc "exports to pandoc json")
    ]


app :: IO ()
app = do
  setLocaleEncoding utf8
  parseArgs >>= \case
    Format mf -> do
      txt <- maybe (Text.hGetContents stdin) Text.readFile mf
      case P.runSimpleParser P.pSimpleDoc txt of
        Left e -> do
          Text.hPutStr stderr (Text.pack $ errorBundlePretty e)
          Text.hPutStr stdout txt
        Right doc ->
          Text.hPutStr stdout (S.runSimpleSerializer S.sDoc doc)
    Pandoc mf -> do
      txt <- maybe (Text.hGetContents stdin) Text.readFile mf
      case P.runSimpleParser P.pSimpleDoc txt of
        Left e -> do
          Text.hPutStr stderr (Text.pack $ errorBundlePretty e)
        Right doc ->
          BL.hPutStr stdout $ encode (toPandoc doc)

