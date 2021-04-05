{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Prose.Text.ParserSpec where

-- base
-- import qualified Data.List.NonEmpty as NE

import Data.Functor.Identity

-- Glob
import System.FilePath.Glob (glob)

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- mtl
import Control.Monad.Reader

-- megaparsec
import Text.Megaparsec hiding (failure)

import Prose.Doc
import Prose.Simple
import Prose.Text.Parser
import Prose.Text.Serializer
-- import Prose.Text.Markdown (serialize, SimpleSerializer, sI, sB, sDoc, sBlock,
--                          simpleSerializeConfig)

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import SpecHelper
import Prose.DocSpec
import Prose.Recursion

spec :: SpecWith ()
spec = do
  specInline
  specBlock
  specSectionText
  specSection
  specAllDataFiles

specInline :: Spec
specInline = describe "inline" do
  ex "word" $ Word "word"
  ex "word-" $ Word "word-"
  ex "10,-" $ Number "10,-"
  ex "10" $ Number "10"
  ex "10\\." $ Number "10."
  ex "10.00" $ Number "10.00"
  ex "ex\\." $ Word "ex."
  ex "@ref" $ Reference "ref"

  describe "simple inlines" do
    myTripping 
      (onInl genSimpleR)
      (overInl serializeSimpleR)
      (onInl parseSimpleR)
  
  describe "simple sentences" do
    serializeRoundtrip
      (toSentences genSimple)
      (fromSentences serializeSimple)
      (toSentences parseSimple)

 where
   ex txt rst = it ("should parse " ++ show txt) do
    case runParser (onInl parseSimpleR <* eof) "hello" txt of
      Left err -> expectationFailure (errorBundlePretty err)
      Right d -> d `shouldBe` Inline' rst

specBlock :: Spec
specBlock = describe "block" do
  describe "single blocks" do
    myTripping 
      (toBlock genSimple) 
      (fromBlock serializeSimple)
      (toBlock parseSimple)

specSectionText :: Spec
specSectionText = describe "section text" do
  let parser = runReaderT pSectionText defaultParserConfig
  it "should not split code blocks" do
    let codeblockexample = "# h1\n```\nblk\n## h2\n```"
    parseOrFail parser codeblockexample \a -> do
      a `shouldSatisfy` (== 1) . length

  onFile "test/data/good/simple.prs" \txt -> do
    it "should contain 2 headers" do
      parseOrFail parser txt \a -> do
        a `shouldSatisfy` (== 2) . length

  onGoodFiles \txt -> do
    it "should parse" do
      parseOrFail parser txt \a -> do
        a `shouldSatisfy` (>= 0) . length
  
  onCanonicalFiles \txt -> do
    it "should parse" do
      parseOrFail parser txt \a -> do
        a `shouldSatisfy` (>= 0) . length


specSection :: Spec
specSection = describe "section" do
  ex "# ,\n"
  "# ," `correctsTo` "# ,\n"

  modifyMaxSuccess (const 20) $
    myTripping 
      (onSec genSimpleR)
      (overSec serializeSimpleR)
      (onSec parseSimpleR)

  where
   ex txt = it ("should parse " ++ show txt) do
    case parse (toSection parseSimple <* eof) "section" txt of
      Left err -> expectationFailure (errorBundlePretty err)
      Right _ -> return ()

   correctsTo txt txt2 = it 
      ("should correct " ++ show txt ++ " to " ++ show txt2) do
    case parse (toSection parseSimple <* eof) "section" txt of
      Left err -> expectationFailure (errorBundlePretty err)
      Right d ->
        fromSection serializeSimple d `shouldBe` txt2

serializeRoundtrip ::
  (Show x, Eq x)
  => Gen x
  -> (x -> Text.Text)
  -> Parser x
  -> Spec
serializeRoundtrip gen s p = prop "can serialize and parse" do
  i <- forAll gen
  let txt = s i
  annotate (Text.unpack txt)
  case runParser (p <* eof) "test" txt of
    Left err -> do
      fail (errorBundlePretty err)
    Right x -> do
      annotateShow x
      txt === s x

myTripping ::
  (Show x, Eq x)
  => Gen x
  -> (x -> Text.Text)
  -> Parser x
  -> Spec
myTripping gen s p = prop "can serialize and parse" do
  i <- forAll gen
  tripping i s (runParser (p <* eof) "test")


specAllDataFiles :: Spec
specAllDataFiles = describe "on **/*.prs" do
 onGoodFiles \txt -> do
   it "can parse - serialize - parse" do
     parseOrFail (onSec parseSimpleR) txt \d -> do
       parseOrFail (onSec parseSimpleR) (overSec serializeSimpleR d)
        (`shouldBe` d)
 onCanonicalFiles \txt -> do
   it "can parse - serialize" do
     parseOrFail (onSec parseSimpleR) txt \d -> do
       overSec serializeSimpleR d `shouldBe` txt

onGoodFiles :: (Text.Text -> SpecWith ()) -> SpecWith ()
onGoodFiles k = do
  files <- runIO (glob "test/data/good/*.prs")
  forM_ files (`onFile` k)

onCanonicalFiles :: (Text.Text -> SpecWith ()) -> SpecWith ()
onCanonicalFiles k = do
  files <- runIO (glob "test/data/canonical/*.prs")
  forM_ files (`onFile` k)

onFile :: FilePath -> (Text.Text -> SpecWith ()) -> SpecWith ()
onFile file k = describe file do
  k =<< runIO (Text.readFile file)

parseOrFail :: Parser i -> Text.Text -> (i -> IO ()) -> IO ()
parseOrFail p txt run = case parse p "p or f" txt of
  Left err -> expectationFailure (errorBundlePretty err)
  Right d -> run d
