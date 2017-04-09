{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Data.Text.Wrap.Internal.Tokens(
      collect
    ) where

import Data.Monoid((<>))
 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import Data.Text.ICU.Char (GeneralCategory(..), GeneralCategory_(..), property)


-- information used while wrapping
data Token = Number Text
           | Letters Text
           | Kana Text
           | Ideograph Text
           | Hyphens Text
           | Punctuation Text
           | Uncategorized Text
           | Empty
  deriving (Show, Eq)


type ICUBreak = ICU.Break ICU.Word


text :: Token -> Text
text (Number t)        = t
text (Letters t)       = t
text (Kana t)          = t
text (Ideograph t)     = t
text (Hyphens t)       = t
text (Punctuation t)   = t
text (Uncategorized t) = t
text Empty             = ""


collect :: Bool -> [ICUBreak] -> [Text]
collect breakOnHyphens = collectTokens breakOnHyphens . mergeHyphens . fmap breakToToken


breakToToken :: ICUBreak -> Token
breakToToken brk = case ICU.brkStatus brk of
                     ICU.Number        -> Number txt
                     ICU.Letter        -> Letters txt
                     ICU.Kana          -> Kana txt
                     ICU.Ideograph     -> Ideograph txt
                     ICU.Uncategorized -> punctuation (T.head txt)
  where txt = ICU.brkBreak brk
        punctuation c | c == '-'        = Hyphens txt
                      | isPunctuation c = Punctuation txt
                      | otherwise       = Uncategorized txt

        isPunctuation c = property GeneralCategory c `elem` validPunctuation


validPunctuation :: [GeneralCategory]
validPunctuation = [ DashPunctuation
                   , StartPunctuation
                   , EndPunctuation
                   , ConnectorPunctuation
                   , OtherPunctuation
                   ]

mergeHyphens :: [Token] -> [Token]
mergeHyphens = mergeHyphens' Empty
  where
    mergeHyphens' !tok []                         = [tok]
    mergeHyphens' Empty (tk:tks)                  = mergeHyphens' tk tks
    mergeHyphens' (Hyphens !h) (Hyphens h' : tks) = mergeHyphens' (Hyphens (h <> h')) tks
    mergeHyphens' !tok (tk:tks)                   = tok : mergeHyphens' tk tks


collectTokens :: Bool -> [Token] -> [Text]
collectTokens breakOnHyphens = collectTokens' Empty
  where
    collectTokens' !tok []                         = [text tok]
    collectTokens' Empty (tk:tks)                  = collectTokens' tk tks
    collectTokens' (Hyphens !h) (Letters ls : tks) = collectTokens' (Letters (h <> ls)) tks
    collectTokens' (Punctuation !p) (Letters ls : tks)
      | p == "\'" || p == "\"" = collectTokens' (Letters (p <> ls)) tks
      | otherwise              = p : collectTokens' (Letters ls) tks
    collectTokens' (Letters !ls) (Hyphens h : Letters ls' : tks)
      | h == "-"  = case breakOnHyphens of
                      False -> collectTokens' (Letters (ls <> h <> ls')) tks
                      True  -> (ls <> h) : collectTokens' (Letters ls') tks
      | otherwise = ls : h : collectTokens' (Letters ls') tks
    collectTokens' (Letters !ls) (Hyphens h : tks)
      | h == "-"  = case breakOnHyphens of
                      True  -> (ls <> h) : collectTokens' Empty tks
                      False -> collectTokens' (Letters (ls <> h)) tks
      | otherwise = ls : collectTokens' (Hyphens h) tks
    collectTokens' (Letters !ls) (Punctuation p : tks)         = collectTokens' (Letters (ls <> p)) tks
    collectTokens' (Number !n) (Hyphens h : Number n' : tks)   = collectTokens' (Number (n <> h <> n')) tks
    collectTokens' (Number !n) (Punctuation p : tks)           = collectTokens' (Number (n <> p)) tks
    collectTokens' (Uncategorized !u) (Uncategorized u' : tks) = collectTokens' (Uncategorized (u <> u')) tks
    collectTokens' !tok (tk:tks)                               = text tok : collectTokens' tk tks
