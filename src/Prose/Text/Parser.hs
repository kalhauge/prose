{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |

 Parse a doc from Markdown.
-}
module Prose.Text.Parser where

-- mtl
import Control.Monad.Reader

-- base

import Control.Category
import Data.Char hiding (Space)
import Data.Functor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Void
import Prelude hiding (id, (.))

-- text
import Data.Text qualified as Text

-- megaparsec
import Text.Megaparsec hiding (label, sepBy1, sepEndBy1, some)
import Text.Megaparsec qualified
import Text.Megaparsec.Char
import Text.Megaparsec.Debug qualified as DBG

-- parser-combinators
import Control.Monad.Combinators.NonEmpty

import Prose.Annotated
import Prose.Builder ()
import Prose.Doc
import Prose.Recursion
import Prose.Simple

parseSimple :: DocCoAlgebra Parser Simple
parseSimple = natCoAlgebra (`runReaderT` defaultParserConfig) alg
 where
  (_, alg) = anaA embedRM (parserR (fmap $ overSec embedR))

parseSimpleR :: Generator Parser Simple
parseSimpleR = fromCoAlgebra embedRM parseSimple

parseAnnotatedR :: Generator Parser (Ann SourcePos)
parseAnnotatedR = gen
 where
  (gen, alg) = anaA' annotate (parserR (overSec annotate))

parseAnnotated :: DocCoAlgebra Parser (Ann SourcePos)
parseAnnotated = natCoAlgebra (`runReaderT` defaultParserConfig) alg
 where
  (_, alg) = anaA' annotate (parserR (overSec annotate))

annotate :: Apply P (Unfix (Ann SourcePos)) ~:> Apply P (Ann SourcePos)
annotate =
  DocMap $
    Instance
      { onSec = \pSec -> do
          pos <- getSourcePos
          AnnSection pos <$> pSec
      , onBlk = \pBlk -> do
          pos <- getSourcePos
          AnnBlock pos <$> pBlk
      , onInl = \pInl -> do
          pos <- getSourcePos
          AnnInline pos <$> pInl
      , onOpenSen = \pSen -> do
          pos <- getSourcePos
          AnnSentence pos <$> pSen
      , onClosedSen = \pSen -> do
          pos <- getSourcePos
          AnnSentence pos <$> pSen
      }

type Parser = Parsec Void Text.Text

data ParserConfig = ParserConfig
  { pCfgActiveQoutes :: [Qoute]
  , pCfgIndent :: Int
  , pCfgSingleLineSentences :: Bool
  , pCfgDepth :: Maybe [String]
  }

defaultParserConfig :: ParserConfig
defaultParserConfig =
  ParserConfig
    { pCfgActiveQoutes = []
    , pCfgSingleLineSentences = False
    , pCfgIndent = 0
    , pCfgDepth = Nothing
    }

type P = ReaderT ParserConfig Parser

label :: Show a => String -> P a -> P a
label name =
  Text.Megaparsec.label name
    . dbg name
    . addDepth name

addDepth :: MonadReader ParserConfig m => String -> m a -> m a
addDepth name =
  local (\a -> a{pCfgDepth = (name :) <$> pCfgDepth a})

dbg :: Show a => String -> P a -> P a
dbg name p = ReaderT \cfg ->
  case pCfgDepth cfg of
    Just d ->
      DBG.dbg (L.intercalate "/" (reverse (name : d))) (runReaderT p cfg)
    Nothing -> runReaderT p cfg

pushActiveQoute ::
  Qoute -> P a -> P a
pushActiveQoute qoute =
  local (\a -> a{pCfgActiveQoutes = qoute : pCfgActiveQoutes a})

indent :: P a -> P a
indent = local (\a -> a{pCfgIndent = pCfgIndent a + 2})

singleline :: P a -> P a
singleline = local (\a -> a{pCfgSingleLineSentences = True})

parserR ::
  forall e.
  ShowR e =>
  (P (Section e) -> P (Sec e)) ->
  Generator P e ->
  DocCoAlgebra P e
parserR pSec app = DocCoAlgebra{..}
 where
  toInline =
    label "inline" $
      choice
        [ char ',' $> Mark Comma
        , char ':' $> Mark Colon
        , string "--" $> Emdash
        , char ';' $> Mark SemiColon
        , char '@' *> (Reference <$> pWord)
        , do
            void $ char '`'
            v <-
              Verbatim
                <$> takeWhileP (Just "verbatim") (\i -> i /= '`' && i /= '\n')
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
            points <- many (string "\\." $> ".")
            return . Number $
              Text.concat (digits : parts)
                <> fromMaybe mempty str
                <> Text.concat points
        , Word <$> pWord
        , Qouted <$> toQoutedSentences
        ]

  toQoutedSentences = try $ do
    qoutedType <- pStartQoute
    qoutedSentences <- pushActiveQoute qoutedType toSentences
    pEndQoute qoutedType
    return $ QoutedSentences{..}

  toSentences :: P (Sentences e)
  toSentences =
    choice
      [ do
          sen <- try $ onClosedSen app
          rest <-
            optional $
              try (pInlineSep *> lookAhead (onInl app))
                *> toSentences
          return $ ClosedSentences sen rest
      , do
          sen <- onOpenSen app
          return $ OpenSentences sen
      ]

  toOpenSentence =
    OpenSentence <$> pInlines

  toClosedSentence = do
    inlines <- pInlines
    ends <- some pEnd
    return $ ClosedSentence inlines ends

  pInlines :: P (NE.NonEmpty (Inl e))
  pInlines = do
    i <- onInl app
    is <- many (try (pInlineSep *> onInl app))
    return $ i NE.:| is

  pEndQoute = \case
    Brackets -> void $ label "end of bracket (])" (char ']')
    DoubleQoute -> void $ label "end of qoute (\")" (char '\"')
    Emph -> void . label "end of emph (/)" $ char '/'
    Strong -> void . label "end of strong (*)" $ char '*'
    Parenthesis -> void . label "end of paranthesis ())" $ char ')'

  pEnd =
    label "end of sentence" $
      choice
        [ char '!' $> Exclamation
        , char '?' $> Question
        , char '.' $> Period
        ]

  pInlineSep =
    asks pCfgSingleLineSentences >>= \case
      True -> hspace
      False -> hspace <* optional (eol *> pIndent)

  pStartQoute = try $ do
    x <-
      choice
        [ char '"' $> DoubleQoute
        , string "*" $> Strong
        , char '/' $> Emph
        , char '(' $> Parenthesis
        , char '[' $> Brackets
        ] -- <* notFollowedBy (endQoute <|> void pEnd)
    qoutes <- asks pCfgActiveQoutes
    if x `elem` qoutes
      then fail "Already included"
      else return x

  pWord = label "a word" do
    c <- letterChar <|> char '$' <|> char '#' <|> char '&'
    txt <-
      many $
        choice
          [ takeWhile1P Nothing \a ->
              isAlphaNum a
                || a == '-'
                || a == '$'
                || a == '#'
                || a == '\''
          , string "\\." $> "."
          ]
    return $ Text.cons c (Text.concat txt)

  toBlock = label "block" do
    c <- choice
      [ comment
      , orderedItems
      , items
      , codeblock
      , para
      ]
    void . optional $ try (many eol *> eof)
    return c
   where
    orderedItems =
      label "ordered items" $
        choice
          [ do
              _ <- lookAhead pNumeralType
              i <- toNumeralItem
              is <-
                many
                  ( try
                      ( optional pEmptyLine
                          *> pIndent
                          *> lookAhead pNumeralType
                      )
                      *> toNumeralItem
                  )
              return $ OrderedItems Numeral (i NE.:| is)
          ]

    codeblock = label "code block" $ do
      _ <- label "start" $ string "```"
      x <- takeWhileP (Just "code block name") (/= '\n')
      let name =
            if Text.null x
              then Nothing
              else Just x
      code <- flip manyTill (try $ eol *> pIndent *> string "```") do
        label "code line" $
          choice
            [ try
                ( eol *> pIndent
                    *> takeWhileP Nothing (/= '\n')
                )
            , try (eol *> lookAhead eol $> "")
            ]
      void eol <|> void eof
      return $ CodeBlock name code

    comment = label "comment" do
      t <- commentLine
      ts <- many (try (eol *> pIndent *> commentLine))
      void eol <|> void eof
      return $ Comment (t : ts)

    commentLine = label "comment line" do
      string "--" *> takeWhileP (Just "a comment line") (/= '\n')

    para = label "paragraph" do
      s <- toSentences
      hspace <* eol <|> eof
      return $ Para s

    items = label "items" do
      i <- toItem
      is <-
        many $
          try
            ( optional pEmptyLine
                *> pIndent
                *> lookAhead pItemType
            )
            *> toItem
      return $ Items (i NE.:| is)

  toOrderedItem =
    choice
      [toNumeralItem]

  toNumeralItem = label "numeral item" do
    _ <- pNumeralType
    let _orderedItemTodo = Nothing
    indent do
      _orderedItemTitle <- toSentences
      dbg "empty" (eof <|> pEmptyLine)
      _orderedItemContents <-
        label "item content" $
          choice
            [ NE.toList <$> pBlocks app
            , pure []
            ]
      pure $ OrderedItem{..}

  pNumeralType =
    try (takeWhile1P Nothing isNumber <* char ')')
      <* hspace1

  toItem = label "item" do
    _itemType <- try (hspace *> pItemType)
    let _itemTodo = Nothing
    indent do
      _itemTitle <- toSentences
      dbg "empty" (eof <|> pEmptyLine)
      _itemContents <-
        label "item content" $
          choice
            [ NE.toList <$> pBlocks app
            , pure []
            ]
      pure $ Item{..}

  pItemType =
    label "an item ('-', '*', or '+')" $
      choice
        [ char '-' $> Minus
        , char '+' $> Plus
        , char '*' $> Times
        ]
        <* hspace1

  toSection :: P (Section e)
  toSection =
    pDoc pSec pSection

  pSection = do
    _sectionTitle <- label "section header" $ singleline toSentences
    choice
      [ do
          let _sectionContent = []
          dbg "eof" eof
          return $ \_sectionSubs -> Section{..}
      , do
          _ <- many pEmptyLine
          _sectionContent <-
            maybe [] NE.toList
              <$> optional (pBlocks app)
          return $ \_sectionSubs -> Section{..}
      ]

pIndent :: P ()
pIndent = do
  n <- asks pCfgIndent
  label ("identation (" ++ show n ++ ")") $ void $ count n (char ' ')

pBlocks :: ShowR e => Generator P e -> P (NE.NonEmpty (Blk e))
pBlocks app =
  label "blocks" $
    some (try (many pEmptyLine *> pIndent) *> onBlk app)

-- Sections

{- | Seperate the file into unparsed sections. This is usefull to
 make parseing more lazy.
-}
pSectionText :: P (NE.NonEmpty (Int, State Text.Text Void))
pSectionText = go id
 where
  go :: (NE.NonEmpty (Int, State Text.Text Void) -> a) -> P a
  go k = do
    header <- Text.length <$> takeWhile1P (Just "header") (== '#')
    hspace
    st <- getParserState
    txt <- Text.intercalate "\n" <$> linesUntilNext True id
    let item = (header, st{stateInput = txt})
    choice
      [ go (k . (item NE.<|))
      , return (k $ item NE.:| [])
      ]

  linesUntilNext :: Bool -> ([Text.Text] -> a) -> P a
  linesUntilNext expectHeader k = do
    txt <- takeWhileP Nothing (/= '\n')
    void eol <|> eof
    let expectHeaderNext =
          if Text.isPrefixOf "```" txt
            then not expectHeader
            else expectHeader
    choice
      [ do
          if expectHeaderNext
            then do
              lookAhead (void (char '#') <|> eof)
            else do
              lookAhead eof
          return (k [txt])
      , do
          linesUntilNext expectHeaderNext (k . (txt :))
      ]

type PState = State Text.Text Void

pDoc ::
  forall e.
  ShowR e =>
  (P (Section e) -> P (Sec e)) ->
  P ([Sec e] -> Section e) ->
  P (Section e)
pDoc pLf pS = do
  x NE.:| rest <- pSectionText
  case pRec x rest of
    (p, []) -> p
    (_, (_, s) : _) -> do
      setParserState s
      fail "unexpected header"
 where
  pRec ::
    (Int, PState) ->
    [(Int, PState)] ->
    (P (Section e), [(Int, PState)])
  pRec (n, s) x = (label "section" p, others)
   where
    (subs, others) = span (\(n', _) -> n < n') x
    p = do
      setParserState s
      p' <- pS
      subs' <- sequence $ split subs
      pure (p' subs')

  split :: [(Int, State Text.Text Void)] -> [P (Sec e)]
  split = \case
    [] -> []
    a : rest ->
      let (p', x') = pRec a rest
       in pLf p' : split x'

pEmptyLine :: P ()
pEmptyLine =
  label "empty line"
    . try
    . void
    $ hspace *> eol
