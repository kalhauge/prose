module Prose.Doc where

-- base
import Prelude hiding (Word)
import qualified Data.List.NonEmpty as NE

-- text
import qualified Data.Text as Text

type Doc = SimpleSection

newtype SimpleSection = SimpleSection
  { getSection :: Section SimpleSection SimpleBlock SimpleInline }
  deriving (Eq)

newtype SimpleBlock = SimpleBlock
  { getBlock :: Block SimpleBlock SimpleInline }
  deriving (Eq)

newtype SimpleInline = SimpleInline
  { getInline :: Inline SimpleInline }
  deriving (Eq)

instance Show SimpleSection where
  showsPrec n (SimpleSection s) =
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show SimpleBlock where
  showsPrec n (SimpleBlock s) =
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show SimpleInline where
  showsPrec n (SimpleInline s) =
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

-- | A Section
data Section s b i = Section
  { sectionTitle :: Sentences i
  , sectionContent :: [b]
  , sectionSubs :: [s]
  } deriving (Show, Eq)

-- | A Block
data Block b i
  = Para (Sentences i)
  | Comment [Text.Text]
  | Items (NE.NonEmpty (Item b i))
  deriving (Eq, Show)

data Item b i = Item
  { itemType :: ItemType
  , itemTodo :: Maybe Bool
  , itemTitle :: Sentences i
  , itemContents :: [b]
  }
  deriving (Eq, Show)

data ItemType
  = Minus
  | Plus
  | Times
  deriving (Eq, Show, Enum, Bounded)

data Sentences i = Sentences
  { closed :: [Sentence i]
  , final :: [i]
  }
  deriving (Eq, Show)

data Sentence i = Sentence
  { sentenceContent :: NE.NonEmpty i
  , sentenceEnd :: NE.NonEmpty End
  }
  deriving (Eq, Show)

data End
  = Exclamation
  | Question
  | Period
  deriving (Eq, Show, Enum, Bounded)

data QoutedSentences i = QoutedSentences
  { qoutedType :: Qoute
  , qoutedSentences :: Sentences i
  }
  deriving (Eq, Show)

data Qoute
  = DoubleQoute
  | Parenthesis
  | Emph
  | Strong
  deriving (Eq, Show, Enum, Bounded)

data Inline i
  = Word Text.Text
  | Broken (NE.NonEmpty Text.Text)
  | Comma
  | Colon
  | SemiColon
  | Verbatim Text.Text
  | Number Text.Text
  | Qouted (QoutedSentences i)
  deriving (Eq, Show)

data DocAlgebra s b i a = DocAlgebra
  { onSections :: s -> a
  , onBlocks :: b -> a
  , onInlines :: i -> a
  }

countSentencesInSimpleInline :: SimpleInline -> Int
countSentencesInSimpleInline (SimpleInline inline) = case inline of
  Qouted q -> countSentencesInQouted countSentencesInSimpleInline q
  _ -> 0

countSentences :: (i -> Int) -> Sentences i -> Int
countSentences countSentencesInInline (Sentences as bs) =
  sum (map countSentencesInSentence as) + (case bs of
    [] -> 0
    _ -> 1 + sum (map countSentencesInInline bs)
  )
 where
  countSentencesInSentence (Sentence is _) =
    1 + sum (fmap countSentencesInInline is)

countSentencesInQouted :: (i -> Int) -> QoutedSentences i -> Int
countSentencesInQouted countSentencesInInline (QoutedSentences _ s) =
  countSentences countSentencesInInline s


