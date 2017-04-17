{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      : Data.Text.Wrap
-- Copyright   : (c) 2016, 2017 Leif Grele
-- License     : BSD3
-- Maintainer  : lgrele+textwrap@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of helpful functions for wrapping and formatting text.
--
module Data.Text.Wrap(
      WrapError(..)
    , WrapperConfig(..)
    , defaultConfig
    , wrap
    , fill
    , fillWith
    , shorten
    , dedent
    , dedentWithLocale
    , indent
    , indentWithLocale
    ) where

import Data.Char(isSpace)
import Data.Function(on)
import Data.List(foldl1', groupBy)
import Data.Maybe(isJust, fromMaybe, fromJust)
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Char

import Prelude hiding (Word)

import Data.Text.Wrap.Internal.Tokens(collect)


-- | A type representing the possible errors produced by
--   running one of the wrapping functions with invalid inputs.
data WrapError = InvalidWidth
               | InvalidTabSize
               | IndentTooLong
               | InvalidLineCount
               | PlaceholderTooLarge
  deriving (Show, Eq)


-- | A collection of config information.
data WrapperConfig =
  WrapperConfig { width              :: Int -- ^ Maximum length of a wrapped line.
                , expandTabs         :: Bool -- ^ If true, all tabs will be replaced with spaces.
                , tabsize            :: Int -- ^ Number of spaces to use for a tab if 'expandTabs' is true.
                , useTabStops        :: Bool -- ^ If true, aligns tabs on nearest multiple of tabsize.
                , replaceWhitespace  :: Bool -- ^ Replace all whitespace with spaces before wrapping.
                , dropWhitespace     :: Bool -- ^ Drop whitespace around lines.
                , initialIndent      :: Text -- ^ Text prepended to the first line.
                , subsequentIndent   :: Text -- ^ Text prepended to lines besides the first.
                , fixSentenceEndings :: Bool -- ^ Attempt to ensure sentences end in two spaces.
                , breakLongWords     :: Bool -- ^ Put words longer than width on multiple lines.
                , breakOnHyphens     :: Bool -- ^ Break on hyphens as well as spaces.
                , maxLines           :: Maybe Int -- ^ If not 'Nothing', truncate to this many lines.
                , placeholder        :: Text -- ^ Text placed after truncated text.
                , locale             :: LocaleName -- ^ Locale of the text, defaults to current locale.
                }

-- | Default config settings.
defaultConfig :: WrapperConfig
defaultConfig = WrapperConfig { width = 70
                              , expandTabs = True
                              , tabsize = 8
                              , useTabStops = True
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


type TokenList = [Text]


validateConfig :: WrapperConfig -> Either WrapError ()
validateConfig cfg
  | width cfg < 1 = Left InvalidWidth
  | tabsize cfg < 0 = Left InvalidTabSize
  | T.length ii >= width cfg = Left IndentTooLong
  | T.length si >= width cfg = Left IndentTooLong
  | isJust (maxLines cfg) = if | maxlines < 1 -> Left InvalidLineCount
                               | plen > width cfg -> Left PlaceholderTooLarge
                               | maxlines == 1 && plen + ilen > width cfg -> Left PlaceholderTooLarge
                               | plen + slen > width cfg -> Left PlaceholderTooLarge
                               | otherwise -> Right ()
  | otherwise = Right ()
  where
    ii = initialIndent cfg
    si = subsequentIndent cfg
    maxlines = fromJust (maxLines cfg)
    plen = T.length (placeholder cfg)
    ilen = T.length (T.stripEnd ii)
    slen = T.length (T.stripEnd si)


-- | Wraps the input text, returning a list of lines no more than 'width'
--   characters long.
wrap :: WrapperConfig -> Text -> Either WrapError [Text]
wrap _ "" = Right []
wrap cfg txt = validateConfig cfg >>
  (return $ wrapChunks $ collect (breakOnHyphens cfg) $ breaks (breakWord (locale cfg)) $ preprocess txt)
  where
    ii = initialIndent cfg
    si = subsequentIndent cfg

    preprocess :: Text -> Text
    preprocess = fixSentences . substitueWhitespace . tabsToSpaces

    tabsToSpaces = if expandTabs cfg
                   then if useTabStops cfg
                        then mconcat . fmap (expandToTabBoundry . T.split (=='\t')) . linebreaks (locale cfg)
                        else T.replace "\t" (T.replicate (tabsize cfg) " ")
                   else id

    expandToTabBoundry []   = ""
    expandToTabBoundry [ln] = ln
    expandToTabBoundry lns  = expandToTabBoundry' (0, "") lns

    expandToTabBoundry' (!len, !text) [ln]     = text <> ln
    expandToTabBoundry' (!len, !text) (ln:lns) = expandToTabBoundry' (len + ilen, text <> ln <> ind) lns
      where
        ind = T.replicate indlen " "
        indlen = (tabsize cfg) - ((len + T.length ln) `mod` (tabsize cfg))
        ilen = indlen + T.length ln

    substitueWhitespace = if replaceWhitespace cfg
                            then T.map (\c -> if isSpace c then ' ' else c)
                            else id

    fixSentences
      | fixSentenceEndings cfg = T.concat . fmap (sentenceWhitespace . brkBreak) . breaks (breakSentence (locale cfg))
      | otherwise = id

    sentenceWhitespace ln = let trailingWhitespace = T.takeWhileEnd isSpace ln
                                breakPart          = T.takeWhileEnd isBreak trailingWhitespace
                             in T.concat [T.dropWhileEnd isSpace ln, "  ", breakPart]


  --TODO: almost everything below here needs a better name


    wrapChunks :: [Text] -> [Text]
    wrapChunks = case maxLines cfg of
      Nothing -> filter (not . T.null) . wrapFull (breakLongWords cfg)
      Just n  -> filter (not . T.null) . wrapLines n (breakLongWords cfg)


    wrapFull :: Bool -> [Text] -> [Text]
    wrapFull _ [] = []
    wrapFull break [c]
      | T.null c' = []
      | break     = catTokens <$> go break (T.length ii, [ii]) [c]
      | otherwise = [ii <> c']
      where c' = maybeStrip c
    wrapFull break cs =
      let (c':cs') = go break (T.length ii, [ii]) cs
          tail     = fmap catTokens cs'
       in case c' of
         [] -> tail
         _  -> (catTokens c') : tail


    -- wrapLines is almost exactly the same as wrapFull,
    -- but it needs to mess with the last line before concating
    -- the tokens, so it's it's own function for now
    wrapLines :: Int -> Bool -> [Text] -> [Text]
    wrapLines _ _ [] = []
    wrapLines n break [c]
      | T.null c' = []
      | break     = handleBreak n (go break (T.length ii, [ii]) [c])
      | otherwise = [ii <> c']
      where c' = maybeStrip c
    wrapLines n break cs =
      let (c':cs') = go break (T.length ii, [ii]) cs
          lines = case c' of
            [] -> cs'
            _  -> c':cs'
       in handleBreak n lines


    -- handle cleanup around the last line when breaking
    handleBreak n lines = case splitAt (n - 1) lines of
      (_, [])          -> catTokens <$> lines
      (_, [_])         -> catTokens <$> lines
      (lines', last:_) -> catTokens <$> lines' <> [fixupEnd last]
      where
        fixupEnd tokens =
        -- TODO: should get length some other way
          let tokens' = dropWhitespaceTokens tokens
              len = sum (T.length <$> tokens')
              pl  = placeholder cfg
           in if len + T.length pl <= width cfg
                then pl : tokens'
                else fixupEnd (tail tokens')


    go :: Bool -> (Int, TokenList) -> [Text] -> [TokenList]
    go _ (!len, text) [] | null text' = []
                         | otherwise  = [text']
      where
        text' = filter (not . T.null) (dropWhitespaceTokens text)
    go break (!len, text) (c:cs)
      -- the next token is too long for one line,
      -- either break it or put it on one line
      | clen > wdth = if break
                        -- don't need to clean up whitespace when breaking
                        then (c1:text) : startNewLine (c2:cs)
                        else if null text'
                               then [c'] : startNewLine cs'
                               else text' : [c'] : startNewLine cs'
      -- the next token fits, doesn't matter if break is set
      | clen + len <= wdth = go break (len + clen, c : text) cs
      -- can't fit next token on this line, start a new one
      | otherwise = if null text'
                      then startNewLine (dropWhitespaceTokens (c:cs))
                      else text' : startNewLine (dropWhitespaceTokens (c:cs))
      where
        clen    = T.length c
        wdth    = width cfg
        (c1,c2) = T.splitAt (wdth - len) c
        text'   = filter (not . T.null) (dropWhitespaceTokens text)
        c'      = maybeStripEnd c
        cs'     = dropWhitespaceTokens cs
        startNewLine = go break (T.length si, [si])


    maybeStrip = if dropWhitespace cfg
                   then T.strip
                   else id

    maybeStripEnd = if dropWhitespace cfg
                      then T.stripEnd
                      else id

    dropWhitespaceTokens = if dropWhitespace cfg
                             then dropWhile (T.any isSpace)
                             else id

    catTokens = T.concat . reverse


-- | Like wrap, but concatenates lines and adds newlines.
fill :: WrapperConfig -> Text -> Either WrapError Text
fill cfg text = fillWith cfg text "\n"


-- | Like wrap, but concatenates lines with the given separator.
--
-- > fill cfg text = fillWith cfg text "\n"
--
fillWith :: WrapperConfig -> Text -> Text -> Either WrapError Text
fillWith cfg text filler = T.intercalate filler <$> wrap cfg text


-- | Truncates input to no more than 'width' characters.
--   All whitespace will be replaced with single spaces.
shorten :: WrapperConfig -> Text -> Either WrapError Text
shorten cfg = let shortenConfig = cfg{maxLines = Just 1}
               in fill shortenConfig . T.intercalate " " . T.words


-- | Remove common leading whitespace from all lines.
--   Lines containing only whitespace are ignored.
dedent :: Text -> Text
dedent = dedentWithLocale Current


-- | Remove common leading whitespace from all lines.
--   Lines containing only whitespace are ignored.
--   Finds line breaks based on the given locale.
dedentWithLocale :: LocaleName -> Text -> Text
dedentWithLocale locale text = mconcat $ fmap applyMargin lns
  where
    lns = linebreaks locale text

    applyMargin :: Text -> Text
    applyMargin line | allWhitespace line = line
                     | otherwise = fromMaybe line (T.stripPrefix margin line)

    margins = takeSpaces <$> filter (T.any (not . isSpace)) lns
    margin = if null margins then "" else foldl1' chooseMargin margins

    takeSpaces = T.takeWhile isSpace . T.dropWhileEnd isBreak

    chooseMargin acc mgn = case T.commonPrefixes acc mgn of
                             Nothing -> T.empty
                             Just (commonMargin,_,_) -> commonMargin


isBreak :: Char -> Bool
isBreak c = case property LineBreak c of
              Nothing -> False
              Just b -> b `elem` editorBreaks


editorBreaks :: [LineBreak]
editorBreaks = [ LineFeed
               , CarriageReturn
               , CombiningMark -- used by old QNX and Atari systems..
               , NextLine
               , MandatoryBreak
               ]


allWhitespace :: Text -> Bool
allWhitespace = not . T.any (not . isSpace)


-- | Add prefix to all lines matching the given predicate.
--   If no predicate is supplied, adds prefix to all lines that don't consist
--   solely of whitespace.
indent :: Maybe (Text -> Bool) -- ^ predicate
       -> Text -- ^ prefix
       -> Text -- ^ text
       -> Text
indent = indentWithLocale Current


-- | Add prefix to all lines matching the given predicate.
--   If no predicate is supplied, adds prefix to all lines that don't consist
--   solely of whitespace.
--   Finds line breaks based on the given locale.
indentWithLocale :: LocaleName -- ^ locale
                 -> Maybe (Text -> Bool) -- ^ predicate
                 -> Text -- ^ prefix
                 -> Text -- ^ text
                 -> Text
indentWithLocale locale pred prefix = mconcat . fmap transform . linebreaks locale
  where
    transform :: Text -> Text
    transform break | pred' break = prefix <> break
                    | otherwise = break

    pred' = fromMaybe defaultPred pred
    defaultPred = not . allWhitespace


-- collect lines by hard breaks
-- soft breaks are accumulated and then attached to the next hard break
-- so [Hard, Soft, Soft, Hard, Hard, Hard, Soft] becomes
-- [Hard, 2Soft+Hard, Hard, Hard, Soft]
linebreaks :: LocaleName -> Text -> [Text]
linebreaks locale = composeLines . concatMap breaksToTexts . groupBy ((==) `on` brkStatus) . breaks (breakLine locale)
  where
    breaksToTexts :: [Break Line] -> [(Text, Line)]
    breaksToTexts [] = []
    breaksToTexts chunks = case brkStatus (head chunks) of
                             Soft -> [(mconcat $ fmap brkBreak chunks, Soft)]
                             Hard -> fmap (\chunk -> (brkBreak chunk, Hard)) chunks

    composeLines :: [(Text, Line)] -> [Text]
    composeLines [] = []
    composeLines [(text, _)] = [text]
    composeLines ((text, Hard):lns) = text : composeLines lns
    composeLines ((text, Soft):ln:lns) = text <> fst ln : composeLines lns
