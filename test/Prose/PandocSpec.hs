{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Prose.PandocSpec where

-- mtl
import Control.Monad.Reader

import SpecHelper

-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range

import Prose.Builder ()
import Prose.DocSpec
import Prose.Pandoc

spec :: Spec
spec = do
  return ()

--   prop "should do something" do
--     i <- forAll $ runReaderT genSimpleSection genConfig
--     let a = toPandoc i
--     a === a
