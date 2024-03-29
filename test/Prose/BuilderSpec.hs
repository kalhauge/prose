{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Prose.BuilderSpec where

-- base
import qualified Data.List.NonEmpty as NE

-- mtl
-- import Control.Monad.Reader (runReaderT)

import Prose.Builder
import Prose.Doc
import Prose.Simple

import Control.Monad.Reader
import Prose.DocSpec
import SpecHelper

spec :: Spec
spec = do
  describe "builder" do
    it "should build a multi-line comment" do
      comment' "hello\nworld"
        `shouldBe` Block' (Comment ["hello", "world"])

    it "should build an itemized list" do
      let i = item' (sb [comma']) []
      items' [i]
        `shouldBe` Block' (Items (i NE.:| []))

  -- describe "sentence builder" do
  --   prop "should be the reverse of sentence" do
  --     i :: Sentences Simple <- forAll $
  --         genSentences genSimpleInline
  --     tripping i toSentenceBuilder (Just . sen' . fmap getInline)

  describe "show" do
    describe "block" do
      comment' "hello\nworld"
        `shouldShow` "comment' \"hello\\nworld\""
 where
  -- para' (sb [ Word "hello", Word "super"]) `shouldShow`
  --   "para' (sb [word' \"hello\",word' \"super\"])"

  -- describe "sentence builder" do
  --   sb [comma'] <! [comma']
  --     `shouldShow`
  --     "sb [comma'] <! [comma']"

  shouldShow a b = it ("should show " ++ show b) do
    show a `shouldBe` b
