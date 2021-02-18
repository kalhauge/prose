{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prose.Text.MarkdownSpec where

-- base
import Data.Char (isPrint)

-- Glob
import System.FilePath.Glob (glob)

-- text
import qualified Data.Text.IO as Text

-- megaparsec
import Text.Megaparsec

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Prose.Text.Markdown
import Prose.Doc

import SpecHelper

spec :: Spec
spec = do
  allDataFiles

  describe "inline" do
    prop "can serialize - parse - serialize" do
      i <- forAll genSimpleInline
      let t = serialize (sInline i)
      case runParser pInline "serialized" t of
        Left err -> fail (errorBundlePretty err)
        Right i2 -> 
          serialize (sInline i2) === t

 where 
  allDataFiles = runIO (glob "test/data/*.md") >>= mapM_ \f -> describe f do 
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


genSimpleInline :: MonadGen m => m SimpleInline
genSimpleInline = SimpleInline <$> genInline undefined genSimpleInline

genInline :: MonadGen m => m b -> m i -> m (Inline b i)
genInline _genB genI = Gen.recursive Gen.choice
  [ do
      txt <- Gen.text (Range.linear 1 32) 
        (Gen.filterT isPrint Gen.unicode)
      return $ Str txt
  , return Space
  ] 
  [ do 
      emph <- Gen.element [Strong, Italic] 
      subs <- Gen.list (Range.linear 1 10) genI
      return $ Emph emph subs
  ]


