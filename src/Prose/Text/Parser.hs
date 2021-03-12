{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Parser where

-- mtl
import Control.Monad.Reader

-- base
import Data.Void
import Data.Maybe
import Data.Functor
import Control.Applicative (Alternative)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Char hiding (Space)

-- text
import qualified Data.Text as Text

-- megaparsec
import Text.Megaparsec hiding (some, sepBy1, sepEndBy1, label)
import Text.Megaparsec qualified
import Text.Megaparsec.Char
import Text.Megaparsec.Debug qualified as DBG

-- parser-combinators
import Control.Monad.Combinators.NonEmpty

import Prose.Doc
import Prose.Builder ()

data ParserConfig b i = ParserConfig
  { pCfgParseB :: Parser b i b
  , pCfgParseI :: Parser b i i
  , pCfgActiveQoutes :: [Qoute]
  , pCfgIndent :: Int
  , pCfgSingleLineSentences :: Bool
  , pCfgDepth :: Maybe [String]
  }

type P b i = ParsecT Void Text.Text (Reader (ParserConfig b i))

newtype Parser b i a = Parser { runMyParser :: P b i a }

deriving via (P b i) instance MonadReader (ParserConfig b i) (Parser b i)
deriving via (P b i) instance MonadFail (Parser b i)
deriving via (P b i) instance MonadParsec Void Text.Text (Parser b i)
deriving via (P b i) instance MonadPlus (Parser b i)
deriving via (P b i) instance Monad (Parser b i)
deriving via (P b i) instance Applicative (Parser b i)
deriving via (P b i) instance Alternative (Parser b i)
deriving via (P b i) instance Functor (Parser b i)

type X b i = (Show b, Show i)

pushActiveQoute ::
  MonadReader (ParserConfig b i) m
  => Qoute
  -> m a
  -> m a
pushActiveQoute qoute =
  local (\a -> a { pCfgActiveQoutes = qoute:pCfgActiveQoutes a })

pI :: Parser b i i
pI = join $ asks pCfgParseI

pB :: Parser b i b
pB = join $ asks pCfgParseB

addDepth :: MonadReader (ParserConfig b i) m => String -> m a -> m a
addDepth name =
  local (\a -> a { pCfgDepth = (name:) <$> pCfgDepth a })

dbg :: Show a => String -> Parser b i a -> Parser b i a
dbg name p = asks pCfgDepth >>= \case
  Just d ->
    Parser (DBG.dbg (L.intercalate "/" (reverse (name:d))) $ runMyParser p)
  Nothing ->
    p

label :: Show a => String -> Parser b i a -> Parser b i a
label name =
  Text.Megaparsec.label name . dbg name . addDepth name

-- | Parses an inline seperator
pInlineSep :: Parser b i ()
pInlineSep = asks pCfgSingleLineSentences >>= \case
  True -> hspace
  False -> hspace <* optional (eol *> pIndent)

pIndent :: Parser b i ()
pIndent = do
  n <- asks pCfgIndent
  label ("identation (" ++ show n ++ ")") $ void $ count n (char ' ')

indent :: Parser b i a -> Parser b i a
indent = local \a -> a
  { pCfgIndent = pCfgIndent a + 2
  }

oneline :: Parser b i a -> Parser b i a
oneline = local \a -> a
  { pCfgSingleLineSentences = True
  }

pEmptyLine :: Parser b i ()
pEmptyLine = label "empty line" . try .  void $ hspace *> eol


--- Parsers
pInline :: X b i => Parser b i (Inline i)
pInline = label "inline" $ choice
  [ char ',' $> Comma
  , char ':' $> Colon
  , char ';' $> SemiColon
  , do
      void $ char '`'
      v <- Verbatim <$> takeWhileP (Just "verbatim") (/= '`')
      void $ char '`'
      return v
  , try $ do
      digits <- takeWhile1P (Just "digits") isNumber
      parts <- many $ try do
        x <- satisfy (\t -> t == ',' || t == '.')
        Text.cons x <$> takeWhile1P (Just "digits") isNumber
      str <- optional $ try do
        x <- satisfy (\t -> t == ',' || t == '.')
        Text.cons x <$> (string "-" <* notFollowedBy pWord)
      return . Number $ Text.concat (digits : parts) <> fromMaybe mempty str
  , Word <$> pWord
  , Qouted <$> pQoutedSentences
  ]
 where
  pWord = label "a word" do
    c <- letterChar <|> char '$'
    txt <- takeWhileP Nothing \a ->
        isAlphaNum a
        || a == '-'
        || a == '$'
        || a == '\''
    return $ Text.cons c txt

-- | This parser will parse inlines until an empty line is found or
-- the next element is not an inline
pInlines :: Parser b i (NE.NonEmpty i)
pInlines = do
  i <- pI
  is <- many (try (pInlineSep *> pI))
  return $ i NE.:| is

-- | This parser will consume sentences and a final newline.
pSentences :: forall b i. Parser b i (Sentences i)
pSentences = do
  inlines <- pInlines
  mEnds <-  many pEnd
  case NE.nonEmpty mEnds of
    Nothing ->
      return $ OpenSentence inlines
    Just ends -> do
      ClosedSentence (Sentence inlines ends)
      <$> optional (try (pInlineSep *> lookAhead pI) *> pSentences)

pQoutedSentences :: Parser b i (QoutedSentences i)
pQoutedSentences = try $ do
  qoutedType <- pStartQoute
  qoutedSentences <- pushActiveQoute qoutedType pSentences
  pEndQoute qoutedType
  return $ QoutedSentences { .. }

pStartQoute :: Parser b i Qoute
pStartQoute = try $ do
  x <- choice
    [ char '"' $> DoubleQoute
    , string "*" $> Strong
    , char '/' $> Emph
    , char '(' $> Parenthesis
    ] -- <* notFollowedBy (endQoute <|> void pEnd)
  qoutes <- asks pCfgActiveQoutes
  if x `elem` qoutes
  then fail "Already included"
  else return x
 -- where
 --  endQoute = void do
 --    many (oneOf [')', '"', '*', '/']) <* (space1 <|> eof)

pEndQoute :: Qoute -> Parser b i ()
pEndQoute = \case
  DoubleQoute -> void $ label "end of qoute (\")" (char '\"')
  Emph -> void . label "end of emph (/)" $ char '/'
  Strong -> void . label "end of strong (*)" $ char '*'
  Parenthesis -> void . label "end of paranthesis ())" $ char ')'

-- Sentence

pEnd :: Parser b i End
pEnd = label "end of sentence" $ choice
  [ char '!' $> Exclamation
  , char '?' $> Question
  , char '.' $> Period
  ]

pBlocks :: Show b => Parser b i (NE.NonEmpty b)
pBlocks = label "blocks" $
  some (try $ many pEmptyLine *> pB)

-- | Parses a single block
pBlock :: X b i => Parser b i (Block b i)
pBlock = label "block" do
  dbg "indent" pIndent
  choice
    [ comment
    , para
    , items
    ]

 where
  comment = label "comment" do
    t <- commentLine
    ts <- many (try (eol *> pIndent *> commentLine))
    void eol <|> void eof
    return $ Comment (t:ts)

  commentLine = label "comment line" do
    string "--" *> takeWhileP (Just "a comment line") (/= '\n')

  para = label "paragraph" do
    s <- pSentences
    hspace <* eol <|> eof
    return $ Para s

  items = label "items" do
    i <- item
    is <- label "more" $ many (try $ optional pEmptyLine *> pIndent *> item)
    return $ Items (i NE.:| is)

  item = label "item" do
    itemType <- try . label "an item ('-', '*', or '+')" $ choice
      [ char '-' $> Minus
      , char '+' $> Plus
      , char '*' $> Times
      ] <* hspace1
    let itemTodo = Nothing
    indent do
      itemTitle <- pSentences
      dbg "empty" (eof <|> pEmptyLine)
      itemContents <- label "item content" $ choice
        [ try $ do
          NE.toList <$> pBlocks
        , pure []
        ]
      pure $ Item {..}

-- Sections

-- | Seperate the file into unparsed sections. This is usefull to
-- make parseing more lazy.
pSectionText :: forall b i. Parser b i (NE.NonEmpty (Int, State Text.Text Void))
pSectionText = go id

 where
  go :: (NE.NonEmpty (Int, State Text.Text Void) -> a)
    -> Parser b i a
  go k = do
    header <- Text.length <$> takeWhile1P (Just "header") (== '#')
    hspace
    st <- getParserState
    txt <- Text.intercalate "\n" <$> linesUntilNext id
    let item = (header, st {stateInput = txt})
    choice
      [ go (k . (item NE.<|))
      , return (k $ item NE.:| [])
      ]

  linesUntilNext :: ([Text.Text] -> a) -> Parser b i a
  linesUntilNext k = do
    txt <- takeWhileP Nothing (/= '\n')
    void eol <|> eof
    choice
      [ do
          lookAhead (void (char '#') <|> eof)
          return (k [txt])
      , do
          linesUntilNext (k . (txt:))
      ]

type PState = State Text.Text Void

pDoc :: forall b i s. Show s => Parser b i ([s] -> s) -> Parser b i s
pDoc pS = do
  x NE.:| rest <- pSectionText
  case pRec x rest of
    (p, []) -> p
    (_, (_, s):_) -> do
      setParserState s
      fail "unexpected header"
 where
  pRec ::
      (Int, PState)
      -> [(Int, PState)]
      -> (Parser b i s, [(Int, PState)])
  pRec (n, s) x = (label "section" p, others)
   where
    (subs, others) = span (\(n', _) -> n < n') x
    p = do
      setParserState s
      p' <- pS
      subs' <- sequence $ split subs
      return (p' subs')

    split :: [(Int, State Text.Text Void)] -> [Parser b i s]
    split = \case
      [] -> []
      a:rest ->
        let (p', x') = pRec a rest
        in p':split x'

pSection :: X b i => Parser b i ([s] -> Section s b i)
pSection = do
  sectionTitle <- label "section header" $ oneline pSentences
  choice
    [ do
        let sectionContent = []
        dbg "eof" eof
        return $ \sectionSubs -> Section { .. }
    , do
        _ <- many pEmptyLine
        sectionContent <- maybe [] NE.toList <$> optional pBlocks
        return $ \sectionSubs -> Section { ..  }
    ]

pSimpleDoc :: SimpleParser Section'
pSimpleDoc = pDoc ((Section' .) <$> pSection) <* eof

type SimpleParser =
  Parser Block' Inline'

simplePaserConfig :: ParserConfig Block' Inline'
simplePaserConfig = ParserConfig
  { pCfgParseI = Inline' <$> pInline
  , pCfgParseB = Block' <$> pBlock
  , pCfgActiveQoutes = []
  , pCfgSingleLineSentences = False
  , pCfgIndent = 0
  , pCfgDepth = Nothing
  }

runSimpleParser ::
  SimpleParser a
  -> Text.Text
  -> Either (ParseErrorBundle Text.Text Void) a
runSimpleParser p txt =
  runReader (runParserT (runMyParser p) "" txt) simplePaserConfig
