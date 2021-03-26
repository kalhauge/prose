{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Prose.Simple where

-- base
import Text.Show
import Control.Category 
import Prelude hiding ((.), id)
import Data.Monoid
import Data.List.NonEmpty qualified as NE

-- text 
import Data.Text qualified as Text

import Prose.Doc
import Prose.Recursion

data Simple
instance DocR Simple where 
  type Sec Simple = Section'
  type Inl Simple = Inline'
  type Blk Simple = Block'
  type OpenSen Simple = Sentence 'Open Simple
  type ClosedSen Simple = Sentence 'Closed Simple


instance OrdR Simple where
instance EqR Simple where
instance ShowR Simple where

type Doc = Section'

newtype Section' = Section'
  { getSection :: Section Simple }
  deriving (Eq, Ord)

instance Show Section' where
  showsPrec = flip $ overSec showSimple

newtype Block' = Block'
  { getBlock :: Block Simple }
  deriving (Eq, Ord)

instance Show Block' where
  showsPrec = flip (overBlk showSimple)

newtype Inline' = Inline'
  { getInline :: Inline Simple }
  deriving (Eq, Ord)

instance Show Inline' where
  showsPrec = flip (overInl showSimple)

type Item' = Item Simple

instance ProjectableR Simple where
  projectR = DocMap $ Instance
    { onSec = \(Section' sec) -> sec
    , onBlk = \(Block' blk) -> blk
    , onInl = \(Inline' inl) -> inl
    , onOpenSen = id
    , onClosedSen = id
    }

instance EmbedableR Simple where
  embedR = DocMap $ Instance
    { onSec = Section'
    , onBlk = Block'
    , onInl = Inline'
    , onOpenSen = id
    , onClosedSen = id
    }

showSimple :: Extractor Simple (Int -> ShowS)
showSimple = extractR showDoc

countSimpleSentences :: Extractor Simple Int
countSimpleSentences = 
  mapV getSum . extractR countSentences

countSimpleWords :: Extractor Simple Int
countSimpleWords = 
  mapV getSum . extractR countWords

-- instance Show Section' where
--   showsPrec n (Section' s) =
--     showParen (n > app_prec) (showString "Section' " . showsPrec (app_prec + 1) s)
--    where app_prec = 10

-- | A default fold, should be overloaded
showDoc :: DocAlgebra (Value (Int -> ShowS)) (Int -> ShowS)
showDoc = DocAlgebra {..}
 where
  fromSection Section {..} n = 
    showParen (n > app_prec) 
    $ showString "sec' " 
    . fromSentences sectionTitle (app_prec + 1) 
    . showChar ' ' . showListWith ($ 0) sectionContent
    . showChar ' ' . showListWith ($ 0) sectionSubs

  fromBlock blk n = case blk of
    Para s -> 
      showParen (n > app_prec) 
      $ showString "para' "
      . fromSentences s (app_prec + 1)
    Comment x -> 
      showParen (n > app_prec) 
      $ showString "comment' "
        . shows (Text.intercalate "\n" x)
    Items its -> 
      showParen (n > app_prec) 
      $ showString "items' "
      . showListWith (`fromItem` n) (NE.toList its)
    OrderedItems x its -> 
      showParen (n > app_prec) 
      $ showString "ordered' "
      . shows x
      . showChar ' '
      . showListWith (`fromOrderedItem` n) (NE.toList its)

  fromItem Item {} = const $ shows ()
    -- fromSentences itemTitle 
    -- <> fold itemContents

  fromOrderedItem OrderedItem {} = const $ shows ()
    -- fromSentences orderedItemTitle
    -- <> fold orderedItemContents

  fromInline i n = showParen (n > app_prec) $ case i of
    Word w -> showString "word' " . shows w
    Reference w -> showString "ref' " . shows w
    Verbatim w -> showString "verb' " . shows w
    Number w -> showString "num' " . shows w
    Mark m -> showString "mark' " . shows m
    Qouted q -> fromQoutedSentences q n


  fromQoutedSentences (QoutedSentences qute sen) n = 
    showParen (n > app_prec) 
    $ showString str 
    . fromSentences sen (app_prec + 1) 
   where
     str = case qute of
       Brackets -> "brackets' "
       Parenthesis -> "parens' "
       DoubleQoute -> "dqoute' "
       Emph -> "emph' "
       Strong -> "strong' "

  fromSentences sens n = case sens of
    OpenSentences sen' -> 
      showParen (n > app_prec) 
      $ showString "OpenSentences " 
      . sen' n
    ClosedSentences sen' rest -> 
      showParen (n > app_prec) 
      $ showString "ClosedSentences " 
      . sen' n 
      . showChar ' '
      . maybe (const id) fromSentences rest n

  fromSentence :: forall b. Sentence b (Value (Int -> ShowS)) -> Int -> ShowS
  fromSentence sen n = case sen of 
    OpenSentence inlines' -> 
      showParen (n > app_prec) 
      $ showString "open' " 
      . showListWith ($0) (NE.toList inlines')
    ClosedSentence inlines' end -> 
      showParen (n > app_prec) 
      $ showString "closed' " 
      . showListWith (\(x :: Int -> ShowS) -> x 0) (NE.toList inlines')
      . showChar ' '
      . showsPrec (app_prec + 1) (NE.toList end)

  app_prec = 10
-- 

-- instance ShowR e => Show (SentenceBuilder e) where
--   showsPrec n = showParen (n > app_prec) . \case
--     AllClosed sens -> handleRest sens []
--     Current i lst ->
--       case NE.nonEmpty lst of
--         Nothing -> showString "sb " . shows (NE.toList i)
--         Just sens -> handleRest sens (NE.toList i)
-- 
--    where
--     handleRest :: NE.NonEmpty (Sen e 'Closed) -> [Inl e] -> ShowS
--     handleRest (ClosedSentences i (e NE.:| ends)  NE.:| ns) m =
--       ( case NE.nonEmpty ends of
--         Just moreEnds -> shows (AllClosed (Sentence i moreEnds NE.:| ns))
--         Nothing -> shows (Current i ns)
--       )
--       . showsEnd e
--       . shows m
-- 
--     app_prec = 10
-- 
--     -- go (Sentence s (e NE.:| _) NE.:| rest)= case NE.nonEmpty rest of
--     --  Nothing -> showString "sb " . shows (NE.toList s) . showsEnd e
--     --  Just rest' -> go rest' . shows (NE.toList s) . showsEnd e
-- 
--     showsEnd :: End -> ShowS
--     showsEnd = \case
--      Exclamation -> showString " <! "
--      Question -> showString " <? "
--      Period -> showString " <. "

-- instance Show Section' where
--   showsPrec n (Section' s) =
--     showParen (n > app_prec) (showString "Section' " . showsPrec (app_prec + 1) s)
--    where app_prec = 10
-- 
-- instance Show Block' where
--   showsPrec n (Block' s) = case s of
--     Comment txt -> showParen (n > app_prec) $
--       showString "comment' " . shows (Text.intercalate "\n" txt)
--     Para p -> showParen (n > app_prec) $
--       showString "para' " . showsPrec (app_prec + 1) (toSentenceBuilder p)
--     Items it -> showParen (n > app_prec) $
--       showString "items' " . showListWith showsItem (NE.toList it)
--     OrderedItems ot it -> showParen (n > app_prec) $
--       showString "ordered' " . shows ot
--       . showChar ' ' . showListWith showsOrderedItem (NE.toList it)
--    where
--     app_prec = 10
--     showsItem = \case
--       Item Minus Nothing x b ->
--         showString "item' "
--         . showsPrec (app_prec + 1) (toSentenceBuilder x)
--         . showChar ' ' . shows b
--       Item Minus (Just t) x b ->
--         showString "todo' " . shows t . showChar ' '
--         . showsPrec (app_prec + 1) (toSentenceBuilder x)
--         . showChar ' ' . shows b
--       i -> shows i
--     showsOrderedItem (OrderedItem t x b) = case t of
--       Nothing ->
--         showString "oitem' "
--         . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
--         . showChar ' ' . shows b
--       Just t' ->
--         showString "otodo' " . shows t' . showChar ' '
--         . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
--         . showChar ' ' . shows b
-- 
-- 
-- instance Show Inline' where
--   showsPrec n (Inline' s) = case s of
--     Comma -> showString "comma'"
--     Colon -> showString "colon'"
--     SemiColon -> showString "semicolon'"
--     Word w -> showParen (n > app_prec) $
--         showString "word' " . showsPrec (app_prec + 1) w
--     Verbatim w -> showParen (n > app_prec) $
--         showString "verbatim' " . showsPrec (app_prec + 1) w
--     Number w -> showParen (n > app_prec) $
--         showString "number' " . showsPrec (app_prec + 1) w
--     Reference r -> showParen (n > app_prec) $
--         showString "ref' " . showsPrec (app_prec + 1) r
--     Qouted w ->
--       showsQoutedSentences n w
--    where app_prec = 10
-- 
-- -- | Shows an inline with an Inline in.
-- showsInline :: Int -> Inline Inline' -> ShowS
-- showsInline n = \case
--   Qouted w -> showsQoutedSentences n w
--   a -> showsPrec n a
-- 
-- showsQoutedSentences :: Int -> QoutedSentences Inline' -> ShowS
-- showsQoutedSentences n (QoutedSentences qute x) =
--   showParen (n > app_prec) $ showString str . showsPrec (app_prec + 1) (toSentenceBuilder x)
--  where
--    app_prec = 10
--    str = case qute of
--      Brackets -> "brackets' "
--      Parenthesis -> "parens' "
--      DoubleQoute -> "dqoute' "
--      Emph -> "emph' "
--      Strong -> "strong' "
-- 
-- showsSentences :: Int -> Sentences Inline' -> ShowS
-- showsSentences n s = showParen (n > app_prec) $
--   showString "sen' " . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder s)
--  where app_prec = 10

