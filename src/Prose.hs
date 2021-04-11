{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Prose where

import Data.Foldable
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- base
import System.IO (stderr, stdin, stdout)

-- pandoc-types
import Text.Pandoc.Definition qualified as PD

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

import Prose.Pandoc
import Prose.Recursion
import Prose.Simple
import Prose.Text.Parser qualified as P
import Prose.Text.Serializer qualified as S

-- import Prose.Extension.Article qualified as Article

data DocumentType = forall a e.
  Show e =>
  DocumentType
  { dtFromSection' :: Section' -> Either e a
  , dtToSection' :: a -> Section'
  , dtToPandoc :: a -> PD.Pandoc
  }

data Config = Config
  { cfgDocumentType :: DocumentType
  , cfgFiles :: Maybe FilePath
  }

data Command
  = Format
  | Pandoc

parseConfig :: Parser Config
parseConfig = do
  cfgDocumentType <-
    choice
      -- [ DocumentType
      --   { dtToPandoc = Article.toPandoc
      --   , dtFromSection' = Article.fromDoc
      --   , dtToSection' = Article.toDoc
      --   } `flag'` long "article"
      [ pure $
          DocumentType
            { dtToPandoc = toPandocDoc
            , dtFromSection' = Right :: a -> Either () a
            , dtToSection' = id
            }
      ]

  cfgFiles <- optional (strOption (long "file" <> metavar "FILE"))

  pure $ Config{..}

parseArgs :: IO (Config, Command)
parseArgs =
  execParser $
    info ((,) <$> parseConfig <*> parseCommand <**> helper) mempty
 where
  parseCommand :: Parser Command
  parseCommand =
    hsubparser $
      fold
        [ command "format" $ info (pure Format) (progDesc "formats the file")
        , command "pandoc" $ info (pure Pandoc) (progDesc "exports to pandoc json")
        ]

app :: IO ()
app = do
  setLocaleEncoding utf8
  (Config{..}, cmd) <- parseArgs
  txt <- maybe (Text.hGetContents stdin) Text.readFile cfgFiles
  handle cfgDocumentType txt cmd
 where
  handle DocumentType{..} txt cmd = do
    mdoc <- case runParser (onSec P.parseSimpleR) "file" txt of
      Left e -> do
        Text.hPutStr stderr (Text.pack $ errorBundlePretty e)
        return Nothing
      Right doc -> case dtFromSection' doc of
        Right s -> return $ Just s
        Left e -> do
          Text.hPutStr stderr (Text.pack $ show e)
          return Nothing

    case cmd of
      Format -> case mdoc of
        Just doc ->
          Text.hPutStr stdout (overSec S.serializeSimpleR (dtToSection' doc))
        Nothing -> Text.hPutStr stdout txt
      Pandoc -> do
        case mdoc of
          Just doc -> BL.hPutStr stdout $ encode (dtToPandoc doc)
          Nothing -> return ()
