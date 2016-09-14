{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Wrap(
      WrapError(..)
    , WrapperConfig(..)
    , defaultConfig
    , wrap
    , fill
    , shorten
    , dedent
    , dedentLocale
    , indent
    , indentWithLocale
    ) where

import Data.Maybe(fromMaybe)
import Data.List(groupBy)
import Data.Function(on)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Types


-- | A type representing the possible errors produced by
-- | running one of the wrapping functions with invalid inputs
data WrapError = InvalidWidth | PlaceholderTooLarge
  deriving (Show, Eq)


-- | A collection of config information
data WrapperConfig =
  WrapperConfig { width              :: Int -- ^ Maximum length of a wrapped line
                , expandTabs         :: Bool -- ^ If true, all tabs will be replaced with spaces
                , tabsize            :: Int -- ^ Number of spaces to use for a tab if 'expandTabs' is true
                , replaceWhitespace  :: Bool -- ^ Replace all whitespace with spaces before wrapping
                , dropWhitespace     :: Bool -- ^ Drop whitespace around lines
                , initialIndent      :: Text -- ^ Text prepended to the first line
                , subsequentIndent   :: Text -- ^ Text prepended to lines besides the first
                , fixSentenceEndings :: Bool -- ^ Attempt to ensure sentences end in two spaces
                , breakLongWords     :: Bool -- ^ Put words longer than width on multiple lines
                , breakOnHyphens     :: Bool -- ^ Break on hyphens as well as spaces
                , maxLines           :: Maybe Int -- ^ If not 'Nothing', truncate to this many lines
                , placeholder        :: Text -- ^ Text placed after truncated text
                , locale             :: LocaleName -- ^ Locale of the text, defaults to current locale
                }

-- | Default config settings
defaultConfig :: WrapperConfig
defaultConfig = WrapperConfig { width = 70
                              , expandTabs = True
                              , tabsize = 8
                              , replaceWhitespace = True
                              , dropWhitespace = True
                              , initialIndent = ""
                              , subsequentIndent = ""
                              , fixSentenceEndings = False
                              , breakLongWords = True
                              , breakOnHyphens = True
                              , maxLines = Nothing
                              , placeholder = " [...]"
                              , locale = Current
                              }

-- | Wraps the input text, returning a list of lines no more than 'width'
-- | characters long
wrap :: WrapperConfig -> Text -> Either WrapError [Text]
wrap = undefined


-- | Like wrap, but concatinates lines and adds newlines
fill :: WrapperConfig -> Text -> Either WrapError Text
fill = undefined


-- | Truncates input to no more than 'width' characters
shorten :: WrapperConfig -> Text -> Either WrapError Text
shorten = undefined


-- | Remove common leading whitespace from all lines
dedent :: Text -> Text
dedent text = undefined


-- | Remove common leading whitespace from all lines
-- | Finds line breaks based on the given locale
dedentWithLocale :: LocaleName -> Text -> Text
dedentWithLocale locale text = undefined


-- | Add 'prefix' to all lines matching the given predicate
-- | If no predicate is supplied, adds 'prefix' to all lines that don't consist
-- | solely of whitespace
indent :: Maybe (Text -> Bool) -> Text -> Text -> Text
indent = indentWithLocale Current


-- | Add 'prefix' to all lines matching the given predicate
-- | If no predicate is supplied, adds 'prefix' to all lines that don't consist
-- | solely of whitespace
-- | Finds line breaks based on the given locale
indentWithLocale :: LocaleName -> Maybe (Text -> Bool) -> Text -> Text -> Text
indentWithLocale locale pred prefix = T.concat . fmap transform . linebreaks . breaks (breakLine locale)
  where
    transform :: Text -> Text
    transform break = if pred' break
                      then prefix `T.append` break
                      else break

    pred' = fromMaybe defaultPred pred
    defaultPred ln = T.strip ln /= T.empty


-- collect lines by hard breaks
-- soft breaks are accumulated and then attached to the next hard break
-- so [Hard, Soft, Soft, Hard, Hard, Hard, Soft] becomes
-- [Hard, 2Soft+Hard, Hard, Hard, Soft]
linebreaks :: [Break Line] -> [Text]
linebreaks = composeLines . concatMap breaksToTexts . groupBy ((==) `on` brkStatus)
  where
    breaksToTexts :: [Break Line] -> [(Text, Line)]
    breaksToTexts [] = []
    breaksToTexts chunks = case brkStatus (head chunks) of
                             Soft -> [(T.concat $ fmap brkBreak chunks, Soft)]
                             Hard -> fmap (\chunk -> (brkBreak chunk, Hard)) chunks

    composeLines :: [(Text, Line)] -> [Text]
    composeLines [] = []
    composeLines [(text, _)] = [text]
    composeLines ((text, Hard):lns) = text : composeLines lns
    composeLines ((text, Soft):ln:lns) = text `T.append` fst ln : composeLines lns
