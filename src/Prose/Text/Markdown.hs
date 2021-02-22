{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Markdown where

-- mtl
import Control.Monad.Reader

-- base
import Data.Void
import Data.String
import Data.Foldable
import Data.Semigroup
import Data.Functor
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Char hiding (Space)

-- text
import qualified Data.Text as Text

-- megaparsec
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Debug as DBG

-- parser-combinators
import Control.Monad.Combinators.NonEmpty

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc as S
import qualified Data.Text.Prettyprint.Doc.Render.Text as S

import Prose.Doc

dbg :: (VisualStream s, ShowErrorComponent e, Show a) =>
             String -> ParsecT e s m a -> ParsecT e s m a
dbg str = 
  if False 
  then DBG.dbg str 
  else id

data ParserConfig s b i = ParserConfig
  { pInline' :: Parser s b i i 
  , pBlock' :: Parser s b i b
  , pActiveQoutes :: [Qoute]
  , pIndention :: Maybe Int
  }

type Parser s b i = ParsecT Void Text.Text (Reader (ParserConfig s b i))

type SimpleParser = Parser SimpleSection SimpleBlock SimpleInline

simplePaserConfig :: ParserConfig SimpleSection SimpleBlock SimpleInline
simplePaserConfig = ParserConfig 
  { pInline' = SimpleInline <$> pInline
  , pBlock' = SimpleBlock <$> pBlock
  , pActiveQoutes = []
  , pIndention = Just 0
  }

pI :: Parser s b i i
pI = join (asks pInline')

pB :: Parser s b i b
pB = join (asks pBlock')

pSingleLine :: Parser b s i a -> Parser b s i a
pSingleLine = local \a -> a
  { pIndention =  Nothing
  }

pIndent :: Parser b s i a -> Parser b s i a
pIndent =  local \a -> a 
  { pIndention = case pIndention a of 
      Just n -> Just $ n + 2
      Nothing -> Just 2
  }

type Serialized = S.Doc ()
type Serializer s b i a = a -> SerializeConfig s b i -> Serialized

data SerializeConfig s b i = SerializeConfig
  { sInline' :: Serializer s b i i
  , sInlineWithSpace' :: i -> Serializer s b i i
  , sBlock' :: Serializer s b i b
  }

instance IsString (SerializeConfig s b i -> Serialized) where
  fromString str = const (fromString str)


type SimpleSerializer a = Serializer SimpleSection SimpleBlock SimpleInline a

simpleSerializeConfig :: SerializeConfig SimpleSection SimpleBlock SimpleInline
simpleSerializeConfig = SerializeConfig 
  { sInline' = sInline . getInline
  , sInlineWithSpace' = \i i2 -> sInlineWithSpace (getInline i) (getInline i2) 
  , sBlock' = sBlock . getBlock
  }

serialize :: Serialized -> Text.Text
serialize = S.renderStrict . S.layoutPretty S.defaultLayoutOptions 

sI :: Serializer s b i i
sI i cfg = sInline' cfg i cfg

sIWS :: i -> Serializer s b i i
sIWS i1 i2 cfg = sInlineWithSpace' cfg i1 i2 cfg

sB :: Serializer s b i b
sB i cfg = sBlock' cfg i cfg

-- Inline

sInline :: Serializer s b i (Inline i)
sInline i cfg = case i of
  Word x -> S.pretty x
  Comma -> ","
  Colon -> ":"
  SemiColon -> ";"
  Hyphen -> "-"
  Qouted s -> sQoutedSentences s cfg

sInlineWithSpace :: Inline i -> Serializer s b i (Inline i)
sInlineWithSpace i i2 = const spaces <> sInline i2
 where 
  spaces = case i2 of
    Word _ -> case i of 
      Hyphen -> mempty 
      _ -> S.space
    Qouted _ -> S.space
    _ -> mempty

pInline :: Show i => Parser s b i (Inline i)
pInline = dbg "inline" . label "inline" $ choice 
  [ char ',' $> Comma
  , char ':' $> Colon
  , char ';' $> SemiColon
  , char '-' $> Hyphen 
  , Qouted <$> pQoutedSentences 
  , Word <$> takeWhile1P (Just "a word") isAlphaNum
  ]

-- Sentence 

pEnd :: Parser s b i End
pEnd = label "end of sentence" $ choice 
  [ char '!' $> Exclamation
  , char '?' $> Question
  , char '.' $> Period 
  ]

sEnd :: Serializer s b i End
sEnd = \case 
  Exclamation -> "!"
  Question -> "?"
  Period -> "."

sInlines :: Serializer s b i [i]
sInlines content = case content of
  c:rest -> 
    sI c <> foldMap (uncurry sIWS) (zip content rest)
  [] -> mempty

sSentence :: Serializer s b i (Sentence i)
sSentence (Sentence contents ends) = 
  sInlines (NE.toList contents) <> foldMap sEnd ends

sSentences :: Serializer s b i (Sentences i)
sSentences (Sentences sens rest) = 
  fold 
    . L.intersperse (const S.hardline) 
    $ map sSentence sens ++ case rest of
        [] -> []
        a -> [sInlines a]

sQoutedSentences :: Serializer s b i (QoutedSentences i)
sQoutedSentences (QoutedSentences qoute sentences) = case qoute of
  SingleQoute -> "'" <> sSentences sentences <> "'"
  DoubleQoute -> "\"" <> sSentences sentences <> "\""
  Parenthesis -> "(" <> sSentences sentences <> ")"
  Emph -> "/" <> sSentences sentences <> "/"
  Strong -> "*" <> sSentences sentences <> "*"


-- pSingleLineSentence :: Parser s b i (Sentence i)
-- pSingleLineSentence = local (\a -> a { pSpace = hspace }) pSentence
-- 
-- pMultiLineSentence :: Int -> Parser s b i (Sentence i)
-- pMultiLineSentence ident = 
--   local (\a -> a { pSpace = (hspace <* void (optional $ eol <* count ident (char ' ')))})
--   pSentence 

pSentences :: Show i => Parser s b i (Sentences i)
pSentences = dbg "sentence" do
  ParserConfig { pInline', pIndention } <- ask 
  let 
    pSpace = dbg "pspace" case pIndention of 
      Just n -> hspace <* optional (void eol <* count n " ")
      Nothing -> hspace
    go sens = do
      x <- pInline'
      r <- many (pSpace *> pInline')
      let i = x NE.:| r
      ends <- dbg "ends" $ many pEnd
      case NE.nonEmpty ends of
        Nothing -> return $ Sentences (reverse sens) (NE.toList i)
        Just ends' -> 
          let sen = Sentence i ends'
          in choice 
            [ do
                _ <- try (pSpace *> lookAhead pInline')
                go (sen:sens)
            , return $ Sentences (reverse (sen:sens)) []
            ]
  go []

pQoutedSentences :: Show i => Parser s b i (QoutedSentences i)
pQoutedSentences = do 
  qoute <- startQoute
  sens <- local (\a -> a { pActiveQoutes = qoute: pActiveQoutes a}) pSentences 
  case qoute of 
    SingleQoute -> void $ label "end of qoute (')" (char '\'')
    DoubleQoute -> void $ label "end of qoute (\")" (char '\"')
    Emph -> void . label "end of emph (/)" $ char '/'
    Strong -> void . label "end of strong (*)" $ char '*'
    Parenthesis -> void . label "end of paranthesis ())" $ char ')'

  return $ QoutedSentences qoute sens

 where 
  startQoute = try $ do 
    qoutes <- asks pActiveQoutes
    x <-  choice 
      [ char '\'' $> SingleQoute
      , char '"' $> DoubleQoute
      , string "*" $> Strong
      , char '/' $> Emph
      , char '(' $> Parenthesis
      ] <* notFollowedBy (endQoute <|> void pEnd)

    if x `elem` qoutes
    then fail "Already included"
    else return x

  -- qouteSymbol = void $ oneOf ['\'', '"', '*']
  endQoute = void do 
    many (oneOf ['\'', '"', '*', '/']) <* (space1 <|> eof)

-- Blocks

-- sSentences :: Serializer i -> Serializer [Sentence i]
-- sSentences sInline' = 
--   foldMap (S.hsep . map sInline' . sentenceContent) 
-- 

pBlock :: Show i => Parser s b i (Block b i)
pBlock = do
  para <- choice 
    [ Para <$> pSentences 
    ]
  hspace
  void (try $ eol <* some eol) <|> (space <* eof)
  return para

sBlock :: Serializer s b i (Block b i)
sBlock = \case 
  Para sb -> sSentences sb 

sBlocks :: SimpleSerializer [SimpleBlock] 
sBlocks = fold 
  . L.intersperse (const (S.hardline <> S.hardline))
  . map sB 

-- Sections

-- | Seperate the file into unparsed sections. This is usefull to 
-- make parseing more lazy.
pSectionText :: forall s b i. Parser s b i (NE.NonEmpty (Int, State Text.Text Void))
pSectionText = go id

 where 
  go :: (NE.NonEmpty (Int, State Text.Text Void) -> a) 
    -> Parser s b i a 
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

  linesUntilNext :: ([Text.Text] -> a) -> Parser s b i a
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

pDoc :: SimpleParser SimpleSection
pDoc = do
  x NE.:| rest <- pSectionText
  case pSection x rest of 
    (p, []) -> p
    (_, (_, s):_) -> do
      setParserState s
      fail "unexpected header" 
 where
  pSection :: 
      (Int, State Text.Text Void) 
      -> [(Int, State Text.Text Void)] 
      -> (SimpleParser SimpleSection, [(Int, State Text.Text Void)])
  pSection (n, s) x = (p, others)
   where 
    (subs, others) = span (\(n', _) -> n < n') x
    p = do
      setParserState s 
      sens <- pSingleLine pSentences
      _ <- some eol

      blocks <- many pB

      secs <- sequence $ split subs

      return . SimpleSection $ Section
        { sectionTitle = sens
        , sectionContent = blocks
        , sectionSubs = secs
        }

    split :: [(Int, State Text.Text Void)] -> [SimpleParser SimpleSection]
    split = \case
      [] -> []
      a:rest -> 
        let (p', x') = pSection a rest
        in p':split x'

  -- go d ((n, state) NE.:| rest) = do
  --   if d == state then
  --   else 
  --     fail $ "header should be " ++ show n ++ " is " ++ show n

sSection :: Int -> SimpleSerializer SimpleSection 
sSection n (SimpleSection sec) cfg = S.vsep $
  [ stimes n "#" S.<+> sSentences (sectionTitle sec) cfg
  , mempty
  , sBlocks (sectionContent sec) cfg
  , mempty
  ] 
  ++
  [ sSection (n + 1) s cfg
  | s <- sectionSubs sec 
  ]

sDoc :: SimpleSerializer SimpleSection
sDoc = sSection 1

-- pSection :: Int -> Parser SimpleSection
-- pSection level = do 
--   _ <- count level $ single '#'
--   _ <- hspace
--   title <- some (pSingleLineSentence pInline)
--   _ <- eol
--   blocks <- many (SimpleBlock <$> pBlock pInline)
--   sections <- many $ pSection (level + 1)
--   return $ SimpleSection (Section title blocks sections)
-- 
-- pDoc :: Parser SimpleSection
-- pDoc = pSection 1 <* eof
-- 
-- 



