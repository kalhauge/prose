{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prose.Text.MarkdownSpec where

-- base
import Data.Functor.Identity

-- mtl
import Control.Monad.Reader

-- Glob
-- import System.FilePath.Glob (glob)

-- text
import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text

-- megaparsec
import Text.Megaparsec hiding (parse)

-- hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

import Prose.Text.Markdown
import Prose.Doc

import Prose.DocSpec 

import SpecHelper

spec :: Spec
spec = do
  specInline
  
  --specSentences
  --specBlock

  -- specAllDataFiles


specInline :: Spec 
specInline = describe "inline" do
  describe "examples" do
    example "world" ( SimpleInline $ Word "world" )
    example "," (SimpleInline Comma)

  serializeRoundtrip (runReaderT genSimpleInline genConfig) sSimpleInline pSimpleInline

 where 
  example = examples pSimpleInline sSimpleInline

  pSimpleInline :: SimpleParser SimpleInline
  pSimpleInline = SimpleInline <$> pInline 

  sSimpleInline :: SimpleSerializer SimpleInline
  sSimpleInline = sInline . getInline


-- specAllDataFiles :: Spec
-- specAllDataFiles = runIO (glob "test/data/*.md") >>= mapM_ \f -> describe f do 
--  txt <- runIO (Text.readFile f)
-- 
--  it "can parse - serialize - parse" do
--    case runParser pDoc f txt of
--      Left err -> expectationFailure (errorBundlePretty err)
--      Right d1 -> do
--        let txt2 = serialize (sDoc d1)
--        case runParser pDoc "serialized" txt2 of
--          Left err -> expectationFailure (errorBundlePretty err)
--          Right d2 -> 
--            d2 `shouldBe` d1


-- specSentences :: Spec 
-- specSentences = describe "sentences" do
--   describe "examples" do
--     example "Hello, world!" $ 
--       Sentence [Word "Hello", Comma, Word "world"] [Exclamation]
-- 
--     parseMulitLineOnly "Hello, \nworld!" $
--       Sentence [Word "Hello", Comma, Word "world"] [Exclamation]
-- 
--     parseNot "Hello, \n\nworld!"
--     parseNot "Hello, \n world!"
-- 
--     parseNotSingleLine "Hello, \nworld!"
-- 
--   serializeRoundtrip 
--     (genSentence genInline) 
--     (sSentence sInlineWithSpace sInline) 
--     (pMultiLineSentence 0 pInline)
-- 
--  where 
--   example = examples (pMultiLineSentence 0 pInline) (sSentence sInlineWithSpace sInline)
-- 
--   parseMulitLineOnly txt a = it ("should parse " <> show txt) $ do 
--     parse (pMultiLineSentence 0 pInline) txt (`shouldBe` a)
--   
--   parseNot :: Text.Text -> Spec
--   parseNot txt = it ("should not parse " <> show txt) $ do
--     shouldNotParse (pMultiLineSentence 0 pInline) txt
--     True `shouldBe` True
--   
--   parseNotSingleLine :: Text.Text -> Spec
--   parseNotSingleLine txt = it ("should not parse " <> show txt) $ do
--     shouldNotParse (pSingleLineSentence pInline) txt
--     True `shouldBe` True
-- 
-- 
-- specBlock :: Spec 
-- specBlock = describe "block" do
--   describe "examples" do
--     parseOnly "Hello, World!\nThis is a \nsentence.\n\n" $
--       Para 
--         [ Sentence [Word "Hello", Comma, Word "World"] [Exclamation]
--         , Sentence [Word "This", Word "is", Word "a", Word "sentence"] [Period]
--         ]
-- 
--  where 
--   parseOnly txt a = it ("should parse " <> show txt) $ do 
--     parse (pBlock pInline) txt (`shouldBe` a)
-- 

examples :: (Show t, Eq t) => SimpleParser t -> SimpleSerializer t -> Text.Text -> t -> Spec
examples p s str item = do
  describe (show str) do
    it "should parse" $ 
      parse p str (`shouldBe` item)
    it "should serialize" $ 
      serialize (s item simpleSerializeConfig) `shouldBe` str

parse :: MonadFail m => SimpleParser i -> Text.Text -> (i -> m ()) -> m ()
parse p txt run = case runReader (runParserT (p <* eof) "" txt) simplePaserConfig of 
  Left err -> fail (errorBundlePretty err)
  Right d -> run d

-- 
-- shouldNotParse :: (Show i, MonadFail m) => Parser i -> Text.Text ->  m ()
-- shouldNotParse p txt = case runParser (p <* eof) "" txt of 
--   Left _ -> return ()
--   Right d -> fail ("parsed it" ++ show d)
-- 
-- 

serializeRoundtrip :: 
  (Show i, Eq i) 
  => GenT Identity i 
  -> SimpleSerializer i 
  -> SimpleParser i 
  -> Spec
serializeRoundtrip gen s p = prop "can serialize and parse" do
  i <- forAll gen
  let txt = serialize $ s i simpleSerializeConfig
  parse p txt \x -> do
    txt === serialize (s x simpleSerializeConfig)

