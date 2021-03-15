{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Prose.Internal.DocParser where

-- base
import Data.Void
import Data.List.NonEmpty qualified as NE

-- megaparsec
import Text.Megaparsec
-- import Text.Megaparsec.Error qualified as E

import Prose.Doc

type DocParser i = Parsec Void [i]

dPara :: DocParser Block' (Sentences Inline')
dPara =  flip token mempty \case
  Block' (Para is) -> return is
  _ -> Nothing

dItems :: DocParser Block' (NE.NonEmpty (Item Block' Inline'))
dItems =  flip token mempty \case
  Block' (Items is) -> return is
  _ -> Nothing

dCompressedItems :: DocParser Block' (NE.NonEmpty (ItemTree Inline'))
dCompressedItems = flip token mempty \case
  Block' (Items is) -> mapM compressItem' is
  _ -> Nothing


instance VisualStream [Block'] where
  showTokens _ _ = "hello"
