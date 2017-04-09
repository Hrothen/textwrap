--------------------------------------------------------------------------------
--
-- Test suite for the textwrap module.
--
-- Original tests written by Greg Ward <gward@python.net>.
-- Converted to PyUnit by Peter Hansen <peter@engcorp.com>.
-- Converted to Hspec by Leif Grele <lgrele@gmail.com>.
-- Currently maintained by Leif Grele.
--
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import Control.Monad(zipWithM_)
import Test.Hspec
import Data.Text.Wrap(WrapperConfig(..))
import qualified Data.Text.Wrap as TW
import Data.Text(Text)
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (q)

main = hspec $
  describe "Textwrap" $ do
    wrapTests
    maxLinesTests
    longWordTests
    indentTests
    dedentTests
    indentTestCases
    shortenTests


testConfig :: WrapperConfig
testConfig = TW.defaultConfig{ width = 45 }

testWrap cfg txt expected = TW.wrap cfg txt `shouldBe` Right expected


testWrapLen :: Int -> Text -> [Text] -> IO ()
testWrapLen len = testWrap testConfig{ width = len }


multiLineString :: Text
multiLineString = [q|This is a paragraph that already has
line breaks.  But some of its lines are much longer than the others,
so it needs to be wrapped.
Some lines are 	tabbed too.
What a mess!
|]

wrapTests :: Spec
wrapTests = describe "wrap" $ do
  describe "Simple" $ do
    let text = "Hello there, how are you this fine day?  I'm glad to hear it!"
    it "wraps to length 12" $
      testWrapLen 12 text
        [ "Hello there,"
        , "how are you"
        , "this fine"
        , "day?  I'm"
        , "glad to hear"
        , "it!" ]

    it "wraps to length 42" $
      testWrapLen 42 text
        [ "Hello there, how are you this fine day?"
        , "I'm glad to hear it!" ]

  describe "Empty String" $ do
    it "returns an empty list when wrapping an empty string" $
      testWrapLen 6 "" []

    it "ignores whitespace settings when wrapping an empty string" $
      testWrap testConfig{ width = 6, dropWhitespace = False } "" []

    it "doesn't indent empty strings" $ do
      testWrap testConfig{ width = 6, initialIndent = "++" } ""  []
      testWrap testConfig{ width = 6, initialIndent = "++", dropWhitespace = False } "" []

  describe "Whitespace" $ do
    it "handles messed up strings" $ do
      let multiLineList =
            [ "This is a paragraph that already has line"
            , "breaks.  But some of its lines are much"
            , "longer than the others, so it needs to be"
            , "wrapped.  Some lines are  tabbed too.  What a"
            , "mess!" ]
      testWrap testConfig{ fixSentenceEndings = True } multiLineString  multiLineList
      TW.fill testConfig{ fixSentenceEndings = True } multiLineString `shouldBe`
        Right (T.intercalate "\n" multiLineList)

    it "expands tabs" $
      testWrapLen 80 "\tTest\tdefault\t\ttabsize."
        ["        Test    default         tabsize."]
    it "expands tabs with custom tab size" $
      testWrap testConfig{ width = 80, tabsize = 4 } "\tTest\tcustom\t\ttabsize."
        ["    Test    custom      tabsize."]

  describe "Sentence Endings" $ do
    it "adds a second space to sentence endings" $ do
      let text = "A short line. Note the single space."
          expect = ["A short line.  Note the single space."]
      testWrap testConfig{ width = 60, fixSentenceEndings = True } text expect
    it "handles tricky endings" $ do
      let cfg = testConfig{ width = 60, fixSentenceEndings = True }
      testWrap cfg "Well, Doctor? What do you think?"
                  ["Well, Doctor?  What do you think?"]
      testWrap cfg "Well, Doctor?\nWhat do you think?"
                  ["Well, Doctor?  What do you think?"]
      testWrap cfg "I say, chaps! Anyone for \"tennis?\"\nHmmph!"
                  ["I say, chaps!  Anyone for \"tennis?\"  Hmmph!"]
      testWrap cfg{ width = 20 } "I say, chaps! Anyone for \"tennis?\"\nHmmph!"
                                ["I say, chaps!", "Anyone for \"tennis?\"", "Hmmph!"]
      testWrap cfg{ width = 20 } "And she said, \"Go to hell!\"\nCan you believe that?"
                                 [ "And she said, \"Go to"
                                 , "hell!\"  Can you"
                                 , "believe that?" ]
      testWrap cfg{ width = 60 } "And she said, \"Go to hell!\"\nCan you believe that?"
                                 ["And she said, \"Go to hell!\"  Can you believe that?"]
      testWrap cfg{ width = 60 } "File stdio.h is nice."
                                ["File stdio.h is nice."]

  describe "Short Lines" $ do
    it "moves endings to make lines longer" $
      testWrapLen 20 "This is a\nshort paragraph."
        [ "This is a short"
        , "paragraph."]
    it "removes endings when paragraph would fit on one line" $
      testWrapLen 40 "This is a\nshort paragraph." ["This is a short paragraph."]

    it "adds prefixes" $ do
      let text = "This is a short line."
      testWrapLen 30 text [text]
      testWrap testConfig{ width = 30, initialIndent = "(1) " } text
        ["(1) This is a short line."]

  describe "Hypenated Lines" $ do
    let text = "this-is-a-useful-feature-for-reformatting-posts-from-tim-peters'ly"
    it "wraps with hyphens" $ do
      testWrapLen  40 text
        [ "this-is-a-useful-feature-for-"
        , "reformatting-posts-from-tim-peters'ly" ]
      testWrapLen 41 text
        [ "this-is-a-useful-feature-for-"
        , "reformatting-posts-from-tim-peters'ly" ]
      testWrapLen 42 text
        [ "this-is-a-useful-feature-for-reformatting-"
        , "posts-from-tim-peters'ly" ]

    it "keeps hyphens when words are longer than width" $ do
      let expect = T.splitOn "|" "this-|is-|a-|useful-|feature-|for-|reformatting-|posts-|from-|tim-|peters'ly"
      testWrap testConfig{ width = 1, breakLongWords = False } text expect

    it "doesn't split hyphenated numbers" $ do
      let text = "Python 1.0.0 was released on 1994-01-26.  Python 1.0.1 was\nreleased on 1994-02-15."
      testWrapLen 30 text
        [ "Python 1.0.0 was released on"
        , "1994-01-26.  Python 1.0.1 was"
        , "released on 1994-02-15." ]
      testWrapLen 40 text
        [ "Python 1.0.0 was released on 1994-01-26."
        , "Python 1.0.1 was released on 1994-02-15." ]
      testWrap testConfig{ width = 1, breakLongWords = False } text (T.words text)
      let shopping = "I do all my shopping at 7-11."
      testWrapLen 25 shopping
        [ "I do all my shopping at"
        , "7-11." ]
      testWrapLen 27 shopping
        [ "I do all my shopping at"
        , "7-11." ]
      testWrapLen 29 shopping [shopping]
      testWrap testConfig{ width = 1, breakLongWords = False } shopping (T.words shopping)

  describe "Em-dashes" $ do
    let text = "Em-dashes should be written -- thus."
    it "handles text with em-dashes" $
      testWrapLen 25 text
        [ "Em-dashes should be"
        , "written -- thus." ]
    it "handles edge-cases" $ do
      testWrapLen 29 text
        [ "Em-dashes should be written"
        , "-- thus." ]
      testWrapLen 30 text
        [ "Em-dashes should be written --"
        , "thus." ]
      testWrapLen 35 text
        [ "Em-dashes should be written --"
        , "thus." ]
      testWrapLen 36 text [text]
    it "treats bad em-dashes correctly" $ do
      let badDash = "You can also do--this or even---this."

      testWrapLen 15 badDash
        [ "You can also do"
        , "--this or even"
        , "---this." ]
      testWrapLen 16 badDash
        [ "You can also do"
        , "--this or even"
        , "---this." ]

      testWrapLen 17 badDash
        [ "You can also do--"
        , "this or even---"
        , "this." ]
      testWrapLen 19 badDash
        [ "You can also do--"
        , "this or even---"
        , "this." ]

      testWrapLen 29 badDash
        [ "You can also do--this or even"
        , "---this." ]
      testWrapLen 31 badDash
        [ "You can also do--this or even"
        , "---this." ]

      testWrapLen 32 badDash
        [ "You can also do--this or even---"
        , "this." ]
      testWrapLen 35 badDash
        [ "You can also do--this or even---"
        , "this." ]

  describe "Unix Options" $
    it "wraps unix command line flags correctly" $ do
      let text = "You should use the -n option, or --dry-run in its long form."

      testWrapLen 20 text
        [ "You should use the"
        , "-n option, or --dry-"
        , "run in its long"
        , "form." ]
      testWrapLen 21 text
        [ "You should use the -n"
        , "option, or --dry-run"
        , "in its long form." ]

      let expect = ["You should use the -n option, or", "--dry-run in its long form."]

      testWrapLen 32 text expect
      testWrapLen 34 text expect
      testWrapLen 35 text expect
      testWrapLen 38 text expect

      testWrapLen 39 text
        [ "You should use the -n option, or --dry-"
        , "run in its long form." ]
      testWrapLen 41 text
        [ "You should use the -n option, or --dry-"
        , "run in its long form." ]

      testWrapLen 42 text
        [ "You should use the -n option, or --dry-run"
        , "in its long form." ]

  describe "Drop Whitespace" $ do
    let cfg = testConfig{ dropWhitespace = False }
    it "preserves whitespace when told to" $
      testWrap cfg{ width = 10 } " This is a    sentence with     much whitespace."
        [ " This is a"
        , "    "
        , "sentence "
        , "with     "
        , "much white"
        , "space." ]
    it "preserves whitespace only strings when told to" $
      testWrap cfg{ width = 6 } "   " ["   "]
    it "indents whitespace only strings" $
      testWrap cfg{ width = 6, initialIndent = "   " } "  " ["     "]
    it "drops all whitespace in whitespace-only strings" $
      testWrapLen 6 "  " []

    it "preserves leading whitespace" $ do
      let text = " This is a sentence with leading whitespace."
      testWrapLen 50 text [text]
      testWrapLen 30 text [" This is a sentence with", "leading whitespace."]

    it "removes empty lines" $ do
      let text = "abcd    efgh"
      testWrap cfg{ width = 6, dropWhitespace = False } text ["abcd", "    ", "efgh"]
      testWrapLen 6 text ["abcd", "efgh"]

    it "doesn't add initial indent if dropping whitespace" $
      testWrap testConfig{ width = 6, initialIndent = "++" } "  " []

    it "doesn't drop whitespace indents" $
      testWrap testConfig{ width = 6, initialIndent = "  ", subsequentIndent = "  " } "abcd efgh"
        ["  abcd", "  efgh"]

  describe "Break On Hyphens" $
    it "respects breakOnHyphens" $ do
      let text = "yaba daba-doo"
      let cfg = testConfig{ width = 10 }
      testWrap cfg text ["yaba daba-", "doo"]
      testWrap cfg{ breakOnHyphens = False } text ["yaba", "daba-doo"]

  describe "Bad Width" $
    it "returns an error for invalid width values" $ do
      let text = "Whatever, it doesn't matter"
      TW.wrap testConfig{ width = 0 } text `shouldBe` Left TW.InvalidWidth
      TW.wrap testConfig{ width = -1 } text `shouldBe` Left TW.InvalidWidth

  describe "Umlauts" $ do
    it "doesn't split on umlauts" $
      testWrapLen 13 "Die Empf\xe4nger-Auswahl"
        ["Die", "Empf\xe4nger-", "Auswahl"]

    it "handles umlauts next to dashes" $
      testWrapLen 7 "aa \xe4\xe4-\xe4\xe4" ["aa \xe4\xe4-", "\xe4\xe4"]


testWrapLines :: Int -> Int -> Text -> [Text] -> IO ()
testWrapLines len lines =
  testWrap testConfig{ width = len, maxLines = Just lines }


maxLinesTests :: Spec
maxLinesTests = describe "Max Lines" $ do
  let text = "Hello there, how are you this fine day?  I'm glad to hear it!"
  describe "Simple" $
    it "truncates everything after the line maximum" $ do
      TW.wrap testConfig{ width = 12, maxLines = Just 0 } text `shouldBe` Left TW.InvalidLineCount
      testWrapLines 12 1 text ["Hello [...]"]
      testWrapLines 12 2 text ["Hello there,", "how [...]"]
      testWrapLines 13 2 text ["Hello there,", "how are [...]"]
      testWrapLines 80 1 text [text]
      testWrapLines 12 6 text
        [ "Hello there,"
        , "how are you"
        , "this fine"
        , "day?  I'm"
        , "glad to hear"
        , "it!" ]

  describe "Spaces" $ do
    it "strips spaces before the placeholder" $
      testWrapLines 12 4 text
        [ "Hello there,"
        , "how are you"
        , "this fine"
        , "day? [...]" ]

    it "puts the placeholder at the start of the line if nothing fits" $
      testWrapLines 7 2 text ["Hello", " [...]"]

    it "doesn't use a placeholder for trailing spaces" $
      testWrapLines 12 6 (text `T.append` T.replicate 10 " ")
        [ "Hello there,"
        , "how are you"
        , "this fine"
        , "day?  I'm"
        , "glad to hear"
        , "it!" ]

  describe "Placeholder" $ do
    let check w lns plc txt expected =
          TW.wrap testConfig{ width = w, maxLines = Just lns, placeholder = plc } txt `shouldBe` Right expected
    it "takes a custom placeholder" $ do
      check 12 1 "..." text ["Hello..."]
      check 12 2 "..." text ["Hello there,", "how are..."]

    it "returns an error when the placeholder and non-whitespace indentation are too long" $ do
      TW.wrap testConfig{ width = 16
                        , maxLines = Just 1
                        , initialIndent = "wat>"
                        , placeholder = " [truncated]..."
                        }
        text `shouldBe` Left TW.PlaceholderTooLarge
      TW.wrap testConfig{ width = 16
                        , maxLines = Just 2
                        , subsequentIndent = "why>"
                        , placeholder = " [truncated]..."
                        }
        text `shouldBe` Left TW.PlaceholderTooLarge

    it "handles long placeholders and whitespace indentation" $ do
      testWrap testConfig{ width = 16
                        , maxLines = Just 2
                        , initialIndent = "    "
                        , subsequentIndent = "  "
                        , placeholder = " [truncated]..."
                        }
        text ["    Hello there,", " [truncated]..."]
      testWrap testConfig{ width = 16
                        , maxLines = Just 1
                        , initialIndent = "   "
                        , subsequentIndent = "   "
                        , placeholder = " [truncated]..."
                        }
        text [" [truncated]..."]
      testWrap testConfig{ width = 80, placeholder = T.replicate 1000 "." } text [text]


longWordTests :: Spec
longWordTests = describe "Long Words" $ do
  let text = [q|Did you say "supercalifragilisticexpialidocious?"
How *do* you spell that odd word, anyways?
|]
  it "breaks up long words" $ do
    testWrapLen 30 text
      [ "Did you say \"supercalifragilis"
      , "ticexpialidocious?\" How *do*"
      , "you spell that odd word,"
      , "anyways?" ]
    testWrapLen 50 text
      [ "Did you say \"supercalifragilisticexpialidocious?\""
      , "How *do* you spell that odd word, anyways?" ]

  it "always breaks *something* off" $
    TW.wrap testConfig{ width = 10, subsequentIndent = T.replicate 15 " " }
      (T.replicate 10 "-" `T.append` "hello") `shouldBe` Left TW.IndentTooLong
        -- [ "----------"
        -- , "               h"
        -- , "               e"
        -- , "               l"
        -- , "               l"
        -- , "               o" ]

  -- Prevent a long word to be wrongly wrapped when the
  -- preceding word is exactly one character shorter than the width
  it "handles edge cases" $
    testWrapLen 12 text
      [ "Did you say "
      , "\"supercalifr"
      , "agilisticexp"
      , "ialidocious?"
      , "\" How *do*"
      , "you spell"
      , "that odd"
      , "word,"
      , "anyways?" ]

  it "doesn't break long words if told not to" $ do
    let cfg = testConfig{ width = 30, breakLongWords = False }
    testWrap cfg text
      [ "Did you say"
      , "\"supercalifragilisticexpialidocious?\""
      , "How *do* you spell that odd"
      , "word, anyways?" ]

  it "truncates long words if they go over the max lines" $
    testWrapLines 12 4 text
      [ "Did you say "
      , "\"supercalifr"
      , "agilisticexp"
      , " [...]" ]


indentTests :: Spec
indentTests = describe "Indenting" $
  describe "Fill" $ do
    let text =[q|This paragraph will be filled, first without any indentation,
and then with some (including a hanging indent).|]
    it "fills with no indentation" $
      TW.fill testConfig{ width = 40 } text `shouldBe` Right
        [q|This paragraph will be filled, first
without any indentation, and then with
some (including a hanging indent).|]

    it "fills with an initial indent" $ do
      let expect =
            [ "     This paragraph will be filled,"
            , "first without any indentation, and then"
            , "with some (including a hanging indent)." ]
          cfg = testConfig{ width = 40, initialIndent = "     " }
      testWrap cfg text expect
      TW.fill cfg text `shouldBe` (Right $ T.intercalate "\n" expect)

    it "fills with a subsequent indent" $ do
      let cfg = testConfig{ width = 40
                          , initialIndent = "  * "
                          , subsequentIndent = "    "
                          }
      TW.fill cfg text `shouldBe` Right
        [q|  * This paragraph will be filled, first
    without any indentation, and then
    with some (including a hanging
    indent).|]


dedentTests :: Spec
dedentTests = describe "Dedent" $ do
  let testUnchanged txt = txt `shouldBe` TW.dedent txt
  describe "No Margin" $ do

    it "doesn't do anything with no lines indented" $
      testUnchanged "Hello there.\nHow are you?\nOh good, I'm glad."

    it "ignores blank lines" $
      testUnchanged "Hello there.\n\nBoo!"

    it "preserves indentation level on subsequent lines" $ do
      testUnchanged "Hello there.\n  This is indented."
      testUnchanged "Hello there.\n\n  Boo!\n"

  describe "Same Indentation Level" $ do

    it "dedents all lines evenly" $
      TW.dedent "  Hello there.\n  How are ya?\n  Oh good." `shouldBe`
        "Hello there.\nHow are ya?\nOh good."

    it "dedents all lines evenly ignoring blank lines" $
      TW.dedent "  Hello there.\n\n  How are ya?\n  Oh good.\n" `shouldBe`
        "Hello there.\n\nHow are ya?\nOh good.\n"

    it "ignores indentation on otherwise blank lines" $
      TW.dedent "  Hello there.\n  \n  How are ya?\n  Oh good.\n" `shouldBe`
        "Hello there.\n  \nHow are ya?\nOh good.\n"

  describe "Uneven Indentation Levels" $ do

    it "preserves relative indentation" $ do
      -- These strings might not go through the QQ correctly
      let text =
            [q|    def foo():
                       while 1:
                           return foo|]
          expect =
            [q|def foo():
                   while 1:
                       return foo|]
      TW.dedent text `shouldBe` expect

    it "ignores blank lines" $
      TW.dedent "  Foo\n    Bar\n\n   Baz\n" `shouldBe`
        "Foo\n  Bar\n\n Baz\n"

    it "ignores whitespace on otherwise blank lines" $
      TW.dedent "  Foo\n    Bar\n \n   Baz\n" `shouldBe`
        "Foo\n  Bar\n \n Baz\n"

  describe "Tabs" $ do

    it "preserves internal tabs when making changes" $
      TW.dedent "  hello\tthere\n  how are\tyou?" `shouldBe`
        "hello\tthere\nhow are\tyou?"

    it "preserves internal tabs when not making changes" $
      TW.dedent "hello\tthere\nhow are\tyou?" `shouldBe`
        "hello\tthere\nhow are\tyou?"

    it "doesn't mangle tabs in the margin" $
      testUnchanged "  hello there\n\thow are you?"

    it "doesn't equate 8 spaces with a tab" $
      testUnchanged "        hello there\n\thow are you?"

    it "only removes whitespace if it can do so uniformly" $ do
      let expect = "hello there\nhow are you?"

      TW.dedent "\thello there\n\thow are you?" `shouldBe` expect
      TW.dedent "  \thello there\n  \thow are you?" `shouldBe` expect
      TW.dedent "  \t  hello there\n  \t  how are you?" `shouldBe` expect
      TW.dedent "  \thello there\n  \t  how are you?" `shouldBe`
        "hello there\n  how are you?"

    it "can have a margin smaller than the smallest indent" $
      TW.dedent "  \thello there\n   \thow are you?\n \tI'm fine, thanks" `shouldBe`
        " \thello there\n  \thow are you?\n\tI'm fine, thanks"


-- Unlike indentTests above, this actually tests the indent function
indentTestCases :: Spec
indentTestCases = describe "Indent" $ do
  let roundtripCases = [ "Hi.\nThis is a test.\nTesting."
                       , "Hi.\nThis is a test.\n\nTesting."
                       , "\nHi.\nThis is a test.\nTesting.\n" ]
      cases = roundtripCases ++ [ "Hi.\r\nThis is a test.\r\nTesting.\r\n"
                                , "\nHi.\r\nThis is a test.\n\r\nTesting.\r\n\n" ]

  it "does nothing if the prefix is empty" $
    mapM_ (\s -> TW.indent Nothing "" s `shouldBe` s) cases

  it "does nothing with an empty prefix and trivial predicate" $
    mapM_ (\s -> TW.indent (Just $ const True) "" s `shouldBe` s) cases

  it "skips lines if told to" $
    mapM_ (\s -> TW.indent (Just $ const False) "    " s `shouldBe` s) cases

  it "roundtrips with dedent when using a whitespace prefix" $
    mapM_ (\s -> TW.dedent (TW.indent Nothing "    " s) `shouldBe` s) cases
  it "roundtrips with dedent when using a tab prefix" $
    mapM_ (\s -> TW.dedent (TW.indent Nothing "\t\t" s) `shouldBe` s) cases
  it "roundtrips with dedent when using a mixed whitespace prefix" $
    mapM_ (\s -> TW.dedent (TW.indent Nothing " \t \t " s) `shouldBe` s) cases

  let indentCases pred pfx = map (TW.indent pred pfx) cases

  it "indents lines with non-whitespace characters correctly" $ do
    let expected = [ "  Hi.\n  This is a test.\n  Testing."
                   , "  Hi.\n  This is a test.\n\n  Testing."
                   , "\n  Hi.\n  This is a test.\n  Testing.\n"
                   , "  Hi.\r\n  This is a test.\r\n  Testing.\r\n"
                   , "\n  Hi.\r\n  This is a test.\n\r\n  Testing.\r\n\n" ]
    zipWithM_ shouldBe (indentCases Nothing "  ") expected

  it "adds a prefix to all lines when told to" $ do
    let expected = [ "  Hi.\n  This is a test.\n  Testing."
                   , "  Hi.\n  This is a test.\n  \n  Testing."
                   , "  \n  Hi.\n  This is a test.\n  Testing.\n"
                   , "  Hi.\r\n  This is a test.\r\n  Testing.\r\n"
                   , "  \n  Hi.\r\n  This is a test.\n  \r\n  Testing.\r\n  \n" ]
    zipWithM_ shouldBe (indentCases (Just $ const True) "  ") expected

  it "adds prefix to whitespace only lines" $ do
    let expected = [ "Hi.\nThis is a test.\nTesting."
                   , "Hi.\nThis is a test.\n  \nTesting."
                   , "  \nHi.\nThis is a test.\nTesting.\n"
                   , "Hi.\r\nThis is a test.\r\nTesting.\r\n"
                   , "  \nHi.\r\nThis is a test.\n  \r\nTesting.\r\n  \n" ]
    zipWithM_ shouldBe (indentCases (Just (T.null . T.strip)) "  ") expected


checkShorten :: Int -> Maybe Text -> Text -> Text -> IO ()
checkShorten width placeholder text expect =
  TW.shorten cfg text `shouldBe` Right expect
    where cfg = case placeholder of
                  Just p  -> testConfig{ width = width, placeholder = p }
                  Nothing -> testConfig{ width = width }

shortenTests :: Spec
shortenTests = describe "Shorten" $ do
  it "shortens simple lines" $ do
    let text = "Hello there, how are you this fine day? I'm glad to hear it!"

    checkShorten 18 Nothing text "Hello there, [...]"
    checkShorten (T.length text) Nothing text text
    checkShorten (T.length text - 1) Nothing text
      "Hello there, how are you this fine day? I'm glad to [...]"

  it "uses a placeholder" $ do
    let text = "Hello there, how are you this fine day? I'm glad to hear it!"
        plh  = Just "$$"

    checkShorten 17 plh text "Hello there,$$"
    checkShorten 18 plh text "Hello there, how$$"
    checkShorten 18 (Just " $$") text "Hello there, $$"
    checkShorten (T.length text) plh text text
    checkShorten (T.length text - 1) plh text
      "Hello there, how are you this fine day? I'm glad to hear$$"

  it "doesn't do anything to an empty sring" $
    checkShorten 6 Nothing "" ""

  describe "Whitespace Collapsing" $ do
    let text =
          [q|This is a  paragraph that  already has
            line breaks and 	 tabs too.|]

    it "collapses multiline strings" $ do

      checkShorten 62 Nothing text
        "This is a paragraph that already has line breaks and tabs too."
      checkShorten 61 Nothing text
        "This is a paragraph that already has line breaks and [...]"

    it "removes inner padding" $ do
      checkShorten 12 Nothing "hello      world!  " "hello world!"
      checkShorten 11 Nothing "hello      world!  " "hello [...]"

    it "removes the leading space from the placeholder when it's on its own line" $
      checkShorten 10 Nothing "hello      world!  " "[...]"

  it "returns nothing when width is too small for the placeholder" $
    TW.shorten testConfig{ width = 8, placeholder = "(.......)" } (T.replicate 20 "x") `shouldBe`
      Left TW.PlaceholderTooLarge

  it "replaces the first word with the placeholder if it's too long" $
    checkShorten 5 Nothing "Helloo" "[...]"
