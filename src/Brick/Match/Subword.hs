-- | An implementation of sub-word string matching
module Brick.Match.Subword (
  Matcher,
  matcher,
  matches
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.RE.TDFA.Text as RE

data Matcher = Matcher RE.RE

-- | Generate a matcher that breaks the search term into words (at whitespace
-- boundaries) and allows any text between the words, as long as the words
-- appear in the target term.
--
-- Note, it probably isn't possible for the construction to fail...
matcher :: String -> Maybe Matcher
matcher s = Matcher <$> RE.compileRegex rxStr
  where
    rxStr = L.intercalate ".*" (map RE.escapeREString (words s))

matches :: Matcher -> T.Text -> Bool
matches (Matcher rx) t = RE.matched (t RE.?=~ rx)
