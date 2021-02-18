{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prose.Text.MarkdownSpec where

-- base
import Data.Functor.Identity

-- Glob
import System.FilePath.Glob (glob)

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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
  specSentences
  -- specAllDataFiles


specAllDataFiles :: Spec
specAllDataFiles = runIO (glob "test/data/*.md") >>= mapM_ \f -> describe f do 
 txt <- runIO (Text.readFile f)

 it "can parse - serialize - parse" do
   case runParser pDoc f txt of
     Left err -> expectationFailure (errorBundlePretty err)
     Right d1 -> do
       let txt2 = serialize (sDoc d1)
       case runParser pDoc "serialized" txt2 of
         Left err -> expectationFailure (errorBundlePretty err)
         Right d2 -> 
           d2 `shouldBe` d1


specInline :: Spec 
specInline = describe "inline" do
  describe "examples" do
    example "world" $ Word "world"
    example "," Comma

  serializeRoundtrip genInline sInline pInline

 where 
  example = examples pInline sInline

specSentences :: Spec 
specSentences = describe "sentences" do
  describe "examples" do
    example "Hello, world!" $ 
      Sentence [Word "Hello", Comma, Word "world"] [Exclamation]

    parseMulitLineOnly "Hello, \nworld!" $
      Sentence [Word "Hello", Comma, Word "world"] [Exclamation]

    parseNot "Hello, \n\nworld!"

    parseNotSingleLine "Hello, \nworld!"

  serializeRoundtrip 
    (genSentence genInline) 
    (sSentence sInlineWithSpace sInline) 
    (pMultiLineSentence pInline)

 where 
  example = examples (pMultiLineSentence pInline) (sSentence sInlineWithSpace sInline)

  parseMulitLineOnly txt a = it ("should parse " <> show txt) $ do 
    parse (pMultiLineSentence pInline) txt (`shouldBe` a)
  
  parseNot :: Text.Text -> Spec
  parseNot txt = it ("should not parse " <> show txt) $ do
    shouldNotParse (pMultiLineSentence pInline) txt
    True `shouldBe` True
  
  parseNotSingleLine :: Text.Text -> Spec
  parseNotSingleLine txt = it ("should not parse " <> show txt) $ do
    shouldNotParse (pSingleLineSentence pInline) txt
    True `shouldBe` True


examples :: (Show t, Eq t) => Parser t -> Serializer t -> Text.Text -> t -> Spec
examples p s str item = do
  describe (show str) do
    it "should parse" $ 
      parse p str (`shouldBe` item)
    it "should serialize" $ 
      serialize (s item) `shouldBe` str


parse :: MonadFail m => Parser i -> Text.Text -> (i -> m ()) -> m ()
parse p txt run = case runParser (p <* eof) "" txt of 
  Left err -> fail (errorBundlePretty err)
  Right d -> run d

shouldNotParse :: (Show i, MonadFail m) => Parser i -> Text.Text ->  m ()
shouldNotParse p txt = case runParser (p <* eof) "" txt of 
  Left _ -> return ()
  Right d -> fail ("parsed it" ++ show d)


serializeRoundtrip :: 
  (Show i, Eq i) 
  => GenT Identity i 
  -> Serializer i 
  -> Parser i 
  -> Spec
serializeRoundtrip gen s p = prop "can serialize and parse" do
 i <- forAll gen
 parse p (serialize $ s i) (=== i)

