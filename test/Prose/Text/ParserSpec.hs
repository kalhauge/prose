{-# LANGUAGE TypeApplications #-}
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
--   specBlock
--   specSectionText
--   specSection

  -- specAllDataFiles

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

  focus $ describe "simple inlines" do
    serializeRoundtrip @Inline'
      (onInl (generate @Simple))
      (overInl simpleSerialized)
      (onInl (defaultParser @Simple))

  -- describe "simple sentences" do
  --   serializeRoundtrip
  --     (runReaderT (genSentences genSimpleInline) genConfig)
  --     sSentences pSentences

 where
   ex txt rst = it ("should parse " ++ show txt) do
    case runParser (onInl (defaultParser @Simple) <* eof) "hello" txt of
      Left err -> expectationFailure (errorBundlePretty err)
      Right d -> d `shouldBe` Inline' rst

-- specBlock :: Spec
-- specBlock = describe "block" do
--   -- canonical "- a.\n  b.\n  - c\n- d"
--   -- "- a\n- b\n- c" `correctsTo` "- a\n- b\n- c"
--   -- "- a\n  - b\n  - d\n- c" `correctsTo` "- a\n\n  - b\n  - d\n\n- c"
-- 
--   describe "items" do
--     myTripping (runReaderT genItemsBlock genConfig) sB pB
-- 
--   describe "single blocks" do
--     myTripping (runReaderT genSimpleBlock genConfig) sB pB
-- 
--   describe "multiple blocks" do
--     myTripping
--       (runReaderT
--         (Gen.nonEmpty (Range.linear 1 3) genSimpleBlock)
--         genConfig
--       )
--       sBlocks
--       pBlocks

 -- where
  -- canonical txt = it ("should parse " ++ show txt) do
  --   parseOrFail (pBlock <* eof) txt \a ->
  --     runSimpleSerializer sBlock a `shouldBe` txt

   -- ex txt = it ("should parse " ++ show txt) do
   --  case runSimpleParser (pSimpleDoc <* eof) txt of
   --    Left err -> expectationFailure (errorBundlePretty err)
   --    Right _ -> return ()

   -- correctsTo txt txt2 = it ("should correct " ++ show txt ++ " to " ++ show txt2) do
   --  case runSimpleParser (pBlock <* eof) txt of
   --    Left err -> expectationFailure (errorBundlePretty err)
   --    Right d ->
   --      serialize (sBlock d simpleSerializeConfig) `shouldBe` txt2

-- specSectionText :: Spec
-- specSectionText = describe "section text" do
--   onFile "test/data/good/simple.prs" \txt -> do
--     it "should contain 2 headers" do
--       parseOrFail pSectionText txt \a -> do
--         a `shouldSatisfy` (== 2) . length
-- 
--   onGoodFiles \txt -> do
--     it "should parse" do
--       parseOrFail pSectionText txt \a -> do
--         a `shouldSatisfy` (>= 0) . length
-- 
-- specSection :: Spec
-- specSection = describe "section" do
--   ex "# ,\n"
--   "# ," `correctsTo` "# ,\n"
-- 
--   modifyMaxSuccess (const 20) $
--     serializeRoundtrip (runReaderT genSimpleSection genConfig)
--       sDoc
--       pSimpleDoc
-- 
--   where
--    ex txt = it ("should parse " ++ show txt) do
--     case runSimpleParser (pSimpleDoc <* eof) txt of
--       Left err -> expectationFailure (errorBundlePretty err)
--       Right _ -> return ()
-- 
--    correctsTo txt txt2 = it ("should correct " ++ show txt ++ " to " ++ show txt2) do
--     case runSimpleParser (pSimpleDoc <* eof) txt of
--       Left err -> expectationFailure (errorBundlePretty err)
--       Right d ->
--         runSimpleSerializer sDoc d `shouldBe` txt2

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


-- specAllDataFiles :: Spec
-- specAllDataFiles = describe "on **/*.prs" do
--  onGoodFiles \txt -> do
--    it "can parse - serialize - parse" do
--      parseOrFail pSimpleDoc txt \d -> do
--        parseOrFail pSimpleDoc (runSimpleSerializer sDoc d)
--         (`shouldBe` d)
--  onCanonicalFiles \txt -> do
--    it "can parse - serialize" do
--      parseOrFail pSimpleDoc txt \d -> do
--        runSimpleSerializer sDoc d `shouldBe` txt

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

-- parseOrFail :: SimpleParser i -> Text.Text -> (i -> IO ()) -> IO ()
-- parseOrFail p txt run = case runSimpleParser p txt of
--   Left err -> expectationFailure (errorBundlePretty err)
--   Right d -> run d
