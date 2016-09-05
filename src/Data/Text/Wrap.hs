{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Wrap where

import Data.Text (Text)
import qualified Data.Text as T
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
dedentLocale :: LocaleName -> Text -> Text
dedentLocale locale text = undefined


-- | Add 'prefix' to all lines matching the given predicate
indent :: Maybe (Text -> Bool) -> Text -> Text -> Text
indent pred prefix text = undefined

-- | Add 'prefix' to all lines matching the given predicate
-- | Finds line breaks based on the given locale
indentWithLocale :: LocaleName -> Maybe (Text -> Bool) -> Text -> Text -> Text
indentWithLocale local pred prefix text = undefined
