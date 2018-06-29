-- | An implementation of sub-word string matching
module Brick.Match.Subword (
  Matcher,
  matcher,
  matches
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Regex.TDFA as RE

data Matcher = Matcher RE.Regex

-- | Generate a matcher that breaks the search term into words (at whitespace
-- boundaries) and allows any text between the words, as long as the words
-- appear in the target term.
--
-- Note, it probably isn't possible for the construction to fail...
matcher :: String -> Maybe Matcher
matcher s = Matcher <$> RE.makeRegexM rxStr
  where
    rxStr = L.intercalate ".*" (map escapeREString (words s))

matches :: Matcher -> T.Text -> Bool
matches (Matcher rx) t = RE.match rx (T.unpack t)

-- | FIXME: Make this more systematic
escapeREString :: String -> String
escapeREString = concatMap replaceREChar
  where
    replaceREChar c =
      case c of
        '\\' -> "\\\\"
        '$' -> "\\$"
        '^' -> "\\^"
        '.' -> "\\."
        _ -> c:[]
