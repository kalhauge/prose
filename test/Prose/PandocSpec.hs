{-# LANGUAGE BlockArguments #-}
module Prose.PandocSpec where


-- mtl
import Control.Monad.Reader

import SpecHelper

-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range

import Prose.Pandoc
import Prose.Builder ()
import Prose.DocSpec


spec :: Spec
spec = do
  prop "should do something" do
    i <- forAll $ runReaderT genSimpleSection genConfig
    let a = toPandoc i
    a === a


