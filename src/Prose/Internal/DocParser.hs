{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prose.Internal.DocParser where

-- base

import Data.List.NonEmpty qualified as NE
import Data.Void

-- megaparsec
import Text.Megaparsec

-- import Text.Megaparsec.Error qualified as E

import Prose.Annotated
import Prose.Doc
import Prose.Recursion

type DocParser i = Parsec Void [i]

type APos = Ann SourcePos
type ABlock = AnnBlock SourcePos

dPara :: DocParser (Blk APos) (Sentences APos)
dPara = flip token mempty \case
  AnnBlock _ (Para is) -> return is
  _ -> Nothing

dItems ::
  DocParser (Item APos) a ->
  DocParser (Blk APos) a
dItems parser = flip token mempty \case
  AnnBlock _ (Items is) ->
    case runParser parser "" (NE.toList is) of
      Left _ -> Nothing
      Right m -> Just m
  _ -> Nothing

dCompressedItems :: DocParser (Blk APos) (NE.NonEmpty (ItemTree APos))
dCompressedItems = flip token mempty \case
  AnnBlock _an (Items is) -> mapM compressItem is
  _ -> Nothing

instance VisualStream [AnnBlock SourcePos] where
  showTokens a _b = show a
