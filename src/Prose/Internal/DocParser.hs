{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE EmptyCase #-}
module Prose.Internal.DocParser where

-- base
import Control.Applicative
import qualified Data.Sequence as Seq

-- lens
import Control.Lens hiding (Simple)

-- megaparsec
import Text.Megaparsec (SourcePos)

import qualified Data.Text as Text

-- mtl
import Control.Monad.State

import Prose.Annotated
import Prose.Doc
import Prose.Internal.Validation
import Prose.Recursion
import Prose.Simple (Simple)

-- type DocParser i = Parsec Void [i]

type APos = Ann MPos
type ABlock = AnnBlock MPos

type MPos = SourcePos

data Fault
  = EndOfList
  | ItemsLeftInList
  | FaultIn Text.Text MPos (Seq.Seq Fault)
  deriving (Eq, Show)

type DocParserInt b =
  ValidationT (State [b]) Fault

newtype DocParser b a = DocParser {runDocParser :: DocParserInt b a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState [b]
    , MonadValidate Fault
    , Alternative
    , MonadPlus
    )
    via (DocParserInt b)

parseAll :: DocParser b a -> [b] -> Validation Fault a
parseAll (DocParser s) bs = case runState (runValidationT s) bs of
  (a, []) -> a
  (Failure err, _ : _) -> Failure err
  (_, _ : _) -> Failure (pure ItemsLeftInList)

pop :: DocParser b b
pop =
  validate =<< state \case
    [] -> (Failure (pure EndOfList), [])
    a : rest -> (Success a, rest)

ptail :: DocParser b [b]
ptail = state (,[])

newtype FirstV a = FirstV { runFirstV :: Validation Fault a }

instance Semigroup (FirstV a) where
  (FirstV a) <> b = case a of 
    Success _ -> FirstV a
    Failure _ -> b

instance Monoid (FirstV a) where
  mempty = FirstV (Failure mempty)

match :: (b -> Validation Fault a) -> DocParser b a
match getter = validate . getter =<< pop

takeIt :: Getting (FirstV a) b a -> b -> Validation Fault a
takeIt gt b = runFirstV . getConst $ gt (Const . FirstV . Success) b

matchL :: Getting (FirstV a) b a -> DocParser b a
matchL l = match (takeIt l)

type ParseLens b a = Getting (FirstV a) b a

-- inBlk :: ParseLens (Blk APos) (Block APos)
-- inBlk fn (AnnBlock a b) = fn b <&> \case 
--   Success x -> Success x
--   Failure err -> Failure (pure $ FaultIn "block" a err)


dPara :: DocParser (Blk APos) (Sentences Simple)
dPara = matchL (unAnnBlk . _Para . to (mapDoc toSimple))


-- dItems ::
--   DocParser (Item APos) a ->
--   DocParser (Blk APos) a
-- dItems parser = flip token mempty \case
--   AnnBlock _ (Items is) ->
--     case runParser parser "" (NE.toList is) of
--       Left _ -> Nothing
--       Right m -> Just m
--   _ -> Nothing
-- 
-- dCompressedItems :: DocParser (Blk APos) (NE.NonEmpty (ItemTree APos))
-- dCompressedItems = flip token mempty \case
--   AnnBlock _an (Items is) -> mapM compressItem is
--   _ -> Nothing
-- 
-- instance VisualStream [AnnBlock SourcePos] where
--   showTokens a _b = show a
