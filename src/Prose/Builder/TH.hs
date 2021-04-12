{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prose.Builder.TH where

-- import qualified Data.Text as Text
--
-- import Language.Haskell.TH.Syntax (lift, Lift)
--
-- import Text.Megaparsec (eof, errorBundlePretty)
--
-- import Prose.Doc
-- import Prose.Builder
-- import Prose.Text.Parser
--
-- -- template-haskell
-- import Language.Haskell.TH.Quote
--
-- deriving instance Lift Inline'
-- deriving instance Lift (Inline Inline')
-- deriving instance Lift (QoutedSentences Inline')
-- deriving instance Lift (Sentences Inline')
-- deriving instance Lift (Sentence Inline')
-- deriving instance Lift End
-- deriving instance Lift Qoute
--
--
-- s :: QuasiQuoter
-- s = let
--   quoteExp txt =
--     case runSimpleParser (pSentences <* eof) (Text.pack txt) of
--       Left err -> error (errorBundlePretty err)
--       Right d -> lift d
--   quoteType = undefined
--   quotePat = undefined
--   quoteDec = undefined
--   in QuasiQuoter { ..  }
--
--
