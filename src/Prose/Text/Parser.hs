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
-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Parser where

-- mtl
import Control.Monad.Reader

-- base
import Data.Void
import Control.Category
import Prelude hiding ((.))
import Data.Maybe
import Data.Functor
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
import Prose.Simple
import Prose.Builder ()
import Prose.Recursion


-- parseSimple :: DocCoAlgebra Parser Simple
-- parseSimple =
--     (parserR (pureR . embedR))

-- defaultParser :: 
--   (ShowR e, EmbedableR e)
--   => Monadic Parser e
-- defaultParser = 
--   natCoAlgebra (`runReaderT` defaultParserConfig)) 
--     parserR embedRM

type Parser = Parsec Void Text.Text

data ParserConfig = ParserConfig
  { pCfgActiveQoutes :: [Qoute]
  , pCfgIndent :: Int
  , pCfgSingleLineSentences :: Bool
  , pCfgDepth :: Maybe [String]
  }

defaultParserConfig :: ParserConfig 
defaultParserConfig = ParserConfig
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
  local (\a -> a { pCfgDepth = (name:) <$> pCfgDepth a })

dbg :: Show a => String -> P a -> P a
dbg name p = ReaderT \cfg -> 
  case pCfgDepth cfg of
    Just d -> 
      DBG.dbg (L.intercalate "/" (reverse (name:d))) (runReaderT p cfg)
    Nothing -> runReaderT p cfg

pushActiveQoute ::
  Qoute -> P a -> P a
pushActiveQoute qoute =
  local (\a -> a { pCfgActiveQoutes = qoute:pCfgActiveQoutes a })

parseSimple :: DocCoAlgebra Parser Simple
parseSimple = natCoAlgebra (`runReaderT` defaultParserConfig) gen
 where
  gen = parserR (anaA embedRM parserR)

defaultParser :: (ShowR e, EmbedableR e) 
  => Generator Parser e
defaultParser = 
  mapDoc (runReaderTR defaultParserConfig) 
  $ anaA embedRM parserR

parserR :: 
  forall e.  ShowR e 
  -- => Unfix e ~:> Apply P e
  -- -> Monadic P e
  => Generator P e
  -> DocCoAlgebra P e
parserR app = DocCoAlgebra {..}
 where
  toInline = label "inline" $ choice
    [ char ',' $> Mark Comma
    , char ':' $> Mark Colon
    , char ';' $> Mark SemiColon
    , char '@' *> (Reference <$> pWord)
    , do
        void $ char '`'
        v <- Verbatim 
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
        return . Number $ Text.concat (digits : parts) 
          <> fromMaybe mempty str 
          <> Text.concat points
    , Word <$> pWord
    , Qouted <$> toQoutedSentences
    ]
  
  toQoutedSentences = try $ do
    qoutedType <- pStartQoute
    qoutedSentences <- pushActiveQoute qoutedType toSentences
    pEndQoute qoutedType
    return $ QoutedSentences { .. }

  -- | This parser will consume sentences and a final newline.
  toSentences :: P (Sentences e)
  toSentences = choice 
    [ do
      sen <- onClosedSen app 
      rest <- optional 
        $ try (pInlineSep *> lookAhead (onInl app)) 
          *> toSentences
      return $ ClosedSentences sen rest
    , do
      sen <- onOpenSen app 
      return $ OpenSentences sen
    ]

  -- pInlines :: P (NE.NonEmpty (Inl e))
  -- pInlines = do
  --   i <- onInl app
  --   is <- many (try (pInlineSep *> onInl app))
  --   return $ i NE.:| is

  pEndQoute = \case
    Brackets -> void $ label "end of bracket (])" (char ']')
    DoubleQoute -> void $ label "end of qoute (\")" (char '\"')
    Emph -> void . label "end of emph (/)" $ char '/'
    Strong -> void . label "end of strong (*)" $ char '*'
    Parenthesis -> void . label "end of paranthesis ())" $ char ')'

  pEnd = label "end of sentence" $ choice
    [ char '!' $> Exclamation
    , char '?' $> Question
    , char '.' $> Period
    ]

  pInlineSep = asks pCfgSingleLineSentences >>= \case
    True -> hspace
    False -> hspace <* optional (eol *> pIndent)

  pIndent = do
    n <- asks pCfgIndent
    label ("identation (" ++ show n ++ ")") $ void $ count n (char ' ')

  pStartQoute = try $ do
    x <- choice
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
    c <- letterChar <|> char '$' <|> char '#'
    txt <- many $ choice
      [ takeWhile1P Nothing \a ->
        isAlphaNum a
        || a == '-'
        || a == '$'
        || a == '#'
        || a == '\''
      , string "\\." $> "."
      ]
    return $ Text.cons c (Text.concat txt)

  -- pBlocks :: Show b => Parser b i (NE.NonEmpty b)
  pBlocks = label "blocks" $
    some (try (many pEmptyLine *> pIndent) *> onBlk app)
  
  -- | Parses a single block
  -- pBlock :: X b i => Parser b i (Block b i)
  toBlock = label "block" do
    choice
      [ comment
      , orderedItems
      , items
      , para
      ]

   where
    orderedItems = label "ordered items" $ choice
      [ do
          _ <- lookAhead pNumeralType
          i <- pNumeralItem
          is <- many 
              (try (optional pEmptyLine *> pIndent *> lookAhead pNumeralType) *> pNumeralItem)
          return $ OrderedItems Numeral (i NE.:| is)
      ]
  
    comment = label "comment" do
      t <- commentLine
      ts <- many (try (eol *> pIndent *> commentLine))
      void eol <|> void eof
      return $ Comment (t:ts)
  
    commentLine = label "comment line" do
      string "--" *> takeWhileP (Just "a comment line") (/= '\n')
  
    para = label "paragraph" do
      s <- toSentences
      hspace <* eol <|> eof
      return $ Para s
  
    items = label "items" do
      i <- item
      is <- many (try (optional pEmptyLine *> pIndent *> lookAhead pItemType) *> item)
      return $ Items (i NE.:| is)
  
    pItemType = label "an item ('-', '*', or '+')" $ choice
      [ char '-' $> Minus
      , char '+' $> Plus
      , char '*' $> Times
      ] <* hspace1
  
    item = label "item" do
      itemType <- try (hspace *> pItemType)
      let itemTodo = Nothing
      indent do
        itemTitle <- pSentences
        dbg "empty" (eof <|> pEmptyLine)
        itemContents <- label "item content" $ choice
          [ NE.toList <$> pBlocks
          , pure []
          ]
        pure $ Item {..}
  
    pNumeralType = try (takeWhile1P Nothing isNumber <* char ')') <* hspace1
  
    pNumeralItem = label "numeral item" do
      _ <- pNumeralType
      let orderedItemTodo = Nothing
      indent do
        orderedItemTitle <- pSentences
        dbg "empty" (eof <|> pEmptyLine)
        orderedItemContents <- label "item content" $ choice
          [ NE.toList <$> pBlocks
          , pure []
          ]
        pure $ OrderedItem {..}

  pEmptyLine = label "empty line" . try .  void $ hspace *> eol

-- data ParserConfig b i = ParserConfig
--   { pCfgParseB :: Parser b i b
--   , pCfgParseI :: Parser b i i
--   , pCfgActiveQoutes :: [Qoute]
--   , pCfgIndent :: Int
--   , pCfgSingleLineSentences :: Bool
--   , pCfgDepth :: Maybe [String]
--   }
-- 
-- type P b i = ParsecT Void Text.Text (Reader (ParserConfig b i))
-- 
-- newtype Parser b i a = Parser { runMyParser :: P b i a }
-- 
-- deriving via (P b i) instance MonadReader (ParserConfig b i) (Parser b i)
-- deriving via (P b i) instance MonadFail (Parser b i)
-- deriving via (P b i) instance MonadParsec Void Text.Text (Parser b i)
-- deriving via (P b i) instance MonadPlus (Parser b i)
-- deriving via (P b i) instance Monad (Parser b i)
-- deriving via (P b i) instance Applicative (Parser b i)
-- deriving via (P b i) instance Alternative (Parser b i)
-- deriving via (P b i) instance Functor (Parser b i)
-- 
-- type X b i = (Show b, Show i)
-- 
-- 
-- pI :: Parser b i i
-- pI = join $ asks pCfgParseI
-- 
-- pB :: Parser b i b
-- pB = join $ asks pCfgParseB
-- 
-- 
-- 
-- -- | Parses an inline seperator
-- 
-- 
-- indent :: Parser b i a -> Parser b i a
-- indent = local \a -> a
--   { pCfgIndent = pCfgIndent a + 2
--   }
-- 
-- oneline :: Parser b i a -> Parser b i a
-- oneline = local \a -> a
--   { pCfgSingleLineSentences = True
--   }
-- 
-- 
-- 
-- --- Parsers
-- pInline :: X b i => Parser b i (Inline i)
-- pInline = label "inline" $ choice
--   [ char ',' $> Comma
--   , char ':' $> Colon
--   , char ';' $> SemiColon
--   , char '@' *> (Reference <$> pWord)
--   , do
--       void $ char '`'
--       v <- Verbatim <$> takeWhileP (Just "verbatim") (\i -> i /= '`' && i /= '\n')
--       void $ char '`'
--       return v
--   , try $ do
--       digits <- takeWhile1P (Just "digits") isNumber
--       parts <- many $ try do
--         x <- satisfy (\t -> t == ',' || t == '.')
--         Text.cons x <$> takeWhile1P (Just "digits") isNumber
--       str <- optional $ try do
--         x <- satisfy (\t -> t == ',' || t == '.')
--         Text.cons x <$> (string "-" <* notFollowedBy pWord)
--       points <- many (string "\\." $> ".")
--       return . Number $ Text.concat (digits : parts) <> fromMaybe mempty str <> Text.concat points
--   , Word <$> pWord
--   , Qouted <$> pQoutedSentences
--   ]
--  where
--   pWord = label "a word" do
--     c <- letterChar <|> char '$' <|> char '#'
--     txt <- many $ choice
--       [ takeWhile1P Nothing \a ->
--         isAlphaNum a
--         || a == '-'
--         || a == '$'
--         || a == '#'
--         || a == '\''
--       , string "\\." $> "."
--       ]
--     return $ Text.cons c (Text.concat txt)
-- 
-- -- | This parser will parse inlines until an empty line is found or
-- -- the next element is not an inline
-- 
-- 
-- 
--  -- where
--  --  endQoute = void do
--  --    many (oneOf [')', '"', '*', '/']) <* (space1 <|> eof)
-- 
-- 
-- -- Sentence
-- 
-- 
-- 
-- -- Sections
-- 
-- -- | Seperate the file into unparsed sections. This is usefull to
-- -- make parseing more lazy.
-- pSectionText :: forall b i. Parser b i (NE.NonEmpty (Int, State Text.Text Void))
-- pSectionText = go id
-- 
--  where
--   go :: (NE.NonEmpty (Int, State Text.Text Void) -> a)
--     -> Parser b i a
--   go k = do
--     header <- Text.length <$> takeWhile1P (Just "header") (== '#')
--     hspace
--     st <- getParserState
--     txt <- Text.intercalate "\n" <$> linesUntilNext id
--     let item = (header, st {stateInput = txt})
--     choice
--       [ go (k . (item NE.<|))
--       , return (k $ item NE.:| [])
--       ]
-- 
--   linesUntilNext :: ([Text.Text] -> a) -> Parser b i a
--   linesUntilNext k = do
--     txt <- takeWhileP Nothing (/= '\n')
--     void eol <|> eof
--     choice
--       [ do
--           lookAhead (void (char '#') <|> eof)
--           return (k [txt])
--       , do
--           linesUntilNext (k . (txt:))
--       ]
-- 
-- type PState = State Text.Text Void
-- 
-- pDoc :: forall b i s. Show s => Parser b i ([s] -> s) -> Parser b i s
-- pDoc pS = do
--   x NE.:| rest <- pSectionText
--   case pRec x rest of
--     (p, []) -> p
--     (_, (_, s):_) -> do
--       setParserState s
--       fail "unexpected header"
--  where
--   pRec ::
--       (Int, PState)
--       -> [(Int, PState)]
--       -> (Parser b i s, [(Int, PState)])
--   pRec (n, s) x = (label "section" p, others)
--    where
--     (subs, others) = span (\(n', _) -> n < n') x
--     p = do
--       setParserState s
--       p' <- pS
--       subs' <- sequence $ split subs
--       return (p' subs')
-- 
--     split :: [(Int, State Text.Text Void)] -> [Parser b i s]
--     split = \case
--       [] -> []
--       a:rest ->
--         let (p', x') = pRec a rest
--         in p':split x'
-- 
-- pSection :: X b i => Parser b i ([s] -> Section s b i)
-- pSection = do
--   sectionTitle <- label "section header" $ oneline pSentences
--   choice
--     [ do
--         let sectionContent = []
--         dbg "eof" eof
--         return $ \sectionSubs -> Section { .. }
--     , do
--         _ <- many pEmptyLine
--         sectionContent <- maybe [] NE.toList <$> optional pBlocks
--         return $ \sectionSubs -> Section { ..  }
--     ]
-- 
-- pSimpleDoc :: SimpleParser Section'
-- pSimpleDoc = pDoc ((Section' .) <$> pSection) <* eof
-- 
-- type SimpleParser =
--   Parser Block' Inline'
-- 
-- 
-- runSimpleParser ::
--   SimpleParser a
--   -> Text.Text
--   -> Either (ParseErrorBundle Text.Text Void) a
-- runSimpleParser p txt =
--   runReader (runParserT (runMyParser p) "" txt) simplePaserConfig
