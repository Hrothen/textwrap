{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Data.Text.Wrap(
      WrapError(..)
    , WrapperConfig(..)
    , defaultConfig
    , wrap
    , fill
    , shorten
    , dedent
    , dedentWithLocale
    , indent
    , indentWithLocale
    ) where

import Data.Char(isSpace)
import Data.Function(on)
import Data.List(foldl1', groupBy, elem)
import Data.Maybe(fromMaybe)
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Types

import Prelude hiding (Word)

-- | A type representing the possible errors produced by
-- | running one of the wrapping functions with invalid inputs
data WrapError = InvalidWidth
               | InvalidTabSize
               | IndentTooLong
               | InvalidLineCount
               | PlaceholderTooLarge
  deriving (Show, Eq)


-- | A collection of config information
data WrapperConfig =
  WrapperConfig { width              :: Int -- ^ Maximum length of a wrapped line
                , expandTabs         :: Bool -- ^ If true, all tabs will be replaced with spaces
                , tabsize            :: Int -- ^ Number of spaces to use for a tab if 'expandTabs' is true
                , useTabStops        :: Bool -- ^ If true, aligns tabs on nearest multiple of tabsize
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


-- information used while wrapping
data Chunk = Chunk Text ChunkType
  deriving (Show, Eq)


-- Equivalent to the status field of an ICU Break,
-- but seperate hyphens from Uncategorized
data ChunkType = Number_
               | Letters_
               | Kana_
               | Ideograph_
               | Hyphens_
               | Punctuation
               | Uncategorized_
               | Empty
  deriving (Show, Eq)


emptyChunk = Chunk "" Empty


breakToChunk :: Break Word -> Chunk
breakToChunk brk = case brkStatus brk of
                     Number -> Chunk txt Number_
                     Letter -> Chunk txt Letters_
                     Kana   -> Chunk txt Kana_
                     Ideograph -> Chunk txt Ideograph_
                     Uncategorized -> punctuation (T.head txt)
  where txt = brkBreak brk
        punctuation c | c == '-' = Chunk txt Hyphens_
                      | isPunctuation c = Chunk txt Punctuation
                      | otherwise = Chunk txt Uncategorized_

        isPunctuation c = property GeneralCategory c `elem` validPunctuation


validPunctuation :: [GeneralCategory]
validPunctuation = [ DashPunctuation
                   , StartPunctuation
                   , EndPunctuation
                   , ConnectorPunctuation
                   , OtherPunctuation
                   ]


-- | Wraps the input text, returning a list of lines no more than 'width'
-- | characters long
wrap :: WrapperConfig -> Text -> Either WrapError [Text]
wrap _ "" = Right []
wrap cfg txt | width cfg < 1 = Left InvalidWidth
             | tabsize cfg < 0 = Left InvalidTabSize
             | T.length ii >= width cfg || T.length si >= width cfg = Left IndentTooLong
             | fromMaybe False ((<1) <$> maxLines cfg) = Left InvalidLineCount
             | otherwise = Right $
                 wrapChunks $  combineChunks $ breaks (breakWord (locale cfg)) $ preprocess txt
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
    expandToTabBoundry lns = expandToTabBoundry' (0, "") lns

    expandToTabBoundry' (!len, !text) [ln] = text <> ln
    expandToTabBoundry' (!len, !text) (ln:lns) = expandToTabBoundry' (len + ilen, text <> ln <> ind) lns
      where
        ind = T.replicate indlen " "
        indlen = (tabsize cfg) - ((len + T.length ln) `mod` (tabsize cfg))
        ilen = indlen + T.length ln

    substitueWhitespace = if replaceWhitespace cfg
                          then T.map (\c -> if isSpace c then ' ' else c)
                          else id

    fixSentences | fixSentenceEndings cfg = T.concat . fmap (sentenceWhitespace . brkBreak) . breaks (breakSentence (locale cfg))
                 | otherwise = id

    sentenceWhitespace ln = let trailingWhitespace = T.takeWhileEnd isSpace ln
                                breakPart = T.takeWhileEnd isBreak trailingWhitespace
                             in T.concat [T.dropWhileEnd isSpace ln, "  ", breakPart]


  --TODO: almost everything below here needs a better name

    mergeHyphens chk@(Chunk !c _) [] = [chk]
    mergeHyphens (Chunk _ Empty) (c:cs) = mergeHyphens c cs
    mergeHyphens (Chunk !h Hyphens_) ((Chunk h' Hyphens_):cs) = mergeHyphens (Chunk (h <> h') Hyphens_) cs
    mergeHyphens chk@(Chunk _ _) (c:cs) = chk : mergeHyphens c cs

    combineChunks :: [Break Word] -> [Text]
    combineChunks = combineChunks' emptyChunk . mergeHyphens emptyChunk . fmap breakToChunk

    combineChunks' :: Chunk -> [Chunk] -> [Text]
    combineChunks' (Chunk !c _) [] = [c]
    combineChunks' (Chunk _ Empty) (c:cs) = combineChunks' c cs
    combineChunks' (Chunk !h Hyphens_) ((Chunk c Letters_):cs) = combineChunks' (Chunk (h <> c) Letters_) cs
                                                               -- | otherwise = h : combineChunks' (Chunk c Letters_) cs
    combineChunks' (Chunk !p Punctuation) ((Chunk w Letters_):cs) | p == "\'" || p == "\"" = combineChunks' (Chunk (p <> w) Letters_) cs
                                                                  | otherwise = p : combineChunks' (Chunk w Letters_) cs
    -- combineChunks' (Chunk !h Hyphens_) ((Chunk h' Hyphens_):cs) = combineChunks' (Chunk (h <> h') Hyphens_) cs
    combineChunks' (Chunk !c Letters_) ((Chunk h Hyphens_):(Chunk c' Letters_):cs) | not (breakOnHyphens cfg) && h == "-" = combineChunks' (Chunk (c <> h <> c') Letters_) cs
                                                                                   | breakOnHyphens cfg && h == "-" = (c <> h) : combineChunks' (Chunk c' Letters_) cs
                                                                                   | otherwise = c : h : combineChunks' (Chunk c' Letters_) cs
    combineChunks' (Chunk !c Letters_) ((Chunk h Hyphens_):cs) | breakOnHyphens cfg && h == "-" = (c <> h) : combineChunks' emptyChunk cs
                                                               | h == "--" = c : combineChunks' (Chunk h Hyphens_) cs
                                                               | not (breakOnHyphens cfg) && h == "-" = combineChunks' (Chunk (c <> h) Letters_) cs
                                                               | otherwise = c : combineChunks' (Chunk h Hyphens_) cs
    combineChunks' (Chunk !c Letters_) ((Chunk p Punctuation):cs) = combineChunks' (Chunk (c <> p) Letters_) cs
    combineChunks' (Chunk !n Number_) ((Chunk h Hyphens_):(Chunk n' Number_):cs) = combineChunks' (Chunk (n <> h <> n') Number_) cs
    combineChunks' (Chunk !n Number_) ((Chunk p Punctuation):cs) = combineChunks' (Chunk (n <> p) Number_) cs
    combineChunks' (Chunk !w Uncategorized_) ((Chunk w' Uncategorized_):cs) = combineChunks' (Chunk (w <> w') Uncategorized_) cs
    combineChunks' (Chunk !c _) (c':cs) = c : combineChunks' c' cs


    wrapChunks :: [Text] -> [Text]
    wrapChunks = case (maxLines cfg, breakLongWords cfg) of
                   (Nothing, False) -> filter (not . T.null) . wrapNoBreak
                   (Nothing, True)  -> filter (not . T.null) . wrapBreak
                   (Just n, False)  -> undefined
                   (Just n, True)   -> undefined

    wrapNoBreak [] = []
    wrapNoBreak [c] | T.null (maybeStrip c) = []
                    | otherwise = [ii <> maybeStrip c]
    wrapNoBreak (c:cs) | T.null c' = fmap (si <>) cs'
                       | otherwise = (ii <> c') : fmap (si <>) cs'
      where (c':cs') = goNoBreak ii (T.length c, c) cs

    wrapBreak [] = []
    wrapBreak [c] | T.null (maybeStrip c) = []
                  | otherwise = [ii <> maybeStrip c] --This is wrong, might need to break
    wrapBreak (c:cs) | T.null c' = fmap (si <>) cs'
                     | otherwise = (ii <> c') : fmap (si <>) cs'
      where (c':cs') = goBreak ii (T.length c, c) cs

    maybeStrip = if dropWhitespace cfg then T.strip else id

    maybeStripEnd = if dropWhitespace cfg then T.stripEnd else id

    dropWhitespaceTokens = if dropWhitespace cfg
                           then dropWhile (T.any isSpace)
                           else id

    goNoBreak :: Text -> (Int, Text) -> [Text] -> [Text]
    goNoBreak i (!len, !text) [] | T.null text = []
                                 | otherwise   = [maybeStripEnd text]
    goNoBreak i (!len, !text) (c:cs) | T.null text = goNoBreak i (clen, c) cs
                                     | clen > wdth = maybeStripEnd text : maybeStrip c : (goNoBreak si (0,"") (dropWhitespaceTokens cs))
                                     | clen + len <= wdth = goNoBreak i (len + clen, text <> c) cs
                                     | otherwise = maybeStripEnd text : (goNoBreak si (0, "") (dropWhitespaceTokens (c:cs)))
      where
        clen = T.length c
        wdth = width cfg - T.length i

    goBreak :: Text -> (Int, Text) -> [Text] -> [Text]
    goBreak i (!len, !text) [] = [maybeStripEnd text]
    goBreak i (!len, !text) (c:cs) | clen > wdth = (text <> c1) : (goBreak si (0, "") (c2:cs))
                                   | clen + len <= wdth = goBreak i (len + clen, text <> c) cs
                                   | otherwise = maybeStripEnd text : (goBreak si (0, "") (dropWhitespaceTokens (c:cs)))
      where
        clen = T.length c
        wdth = width cfg - T.length i
        (c1,c2) = T.splitAt (wdth - len) c

    -- wrap' [] = []
    -- wrap' [c] | T.length c > width cfg + T.length ii && breakLongWords cfg = ii <> c1 : fmap ((si<>) . T.strip) (go (0,"") [c2])
    --           | otherwise = [ii <> c]
    --   where (c1,c2) = wrapLongWord (T.length ii) (T.strip c)
    -- wrap' cs = fmap ((si<>). T.strip) (go (0,"") cs)

    -- go (!len, !text) [] = [text]
    -- go (!len, !text) (c:cs) | clen > wdth && not (breakLongWords cfg) = text : c : go (0,"") cs
    --                         | clen > wdth && breakLongWords cfg = (text <> hd) : go (0, "") (tl:cs)
    --                         | clen + len < wdth = go (len + clen, text <> c) cs
    --                         | otherwise = text : go (0, "") (c:cs)
    --   where clen = T.length c
    --         ilen = T.length si
    --         wdth = width cfg - ilen
    --         (hd,tl) = wrapLongWord len c
 
    -- wrapLongWord :: Int -> Text -> (Text,Text)
    -- wrapLongWord len text = T.splitAt (width cfg - len) text


-- | Like wrap, but concatinates lines and adds newlines
fill :: WrapperConfig -> Text -> Either WrapError Text
fill = undefined


-- | Truncates input to no more than 'width' characters
shorten :: WrapperConfig -> Text -> Either WrapError Text
shorten = undefined


-- | Remove common leading whitespace from all lines
-- | Lines containing only whitespace are ignored
dedent :: Text -> Text
dedent = dedentWithLocale Current


-- | Remove common leading whitespace from all lines
-- | Lines containing only whitespace are ignored
-- | Finds line breaks based on the given locale
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
