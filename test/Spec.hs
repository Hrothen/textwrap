{-
Test suite for the textwrap module.

Original tests written by Greg Ward <gward@python.net>.
Converted to PyUnit by Peter Hansen <peter@engcorp.com>.
Converted to Hspec by Leif Grele <lgrele@gmail.com>.
Currently maintained by Leif Grele.
-}


module Main where
import Test.Hspec
import qualified Data.Text.Wrap as TW
import Data.Text(Text)
import qualified Data.Text as T

main = hspec $ do
  describe "Textwrap" $ do
    wrapTests
    maxLinesTests
    longWordTests
    indentTests
    dedentTests
    shortenTests


wrapTests :: Spec
wrapTests = undefined

maxLinesTests :: Spec
maxLinesTests = undefined

longWordTests :: Spec
longWordTests = undefined

indentTests :: Spec
indentTests = undefined

dedentTests :: Spec
dedentTests = undefined

shortenTests :: Spec
shortenTests = undefined
