{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Criterion.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Text.Wrap as TW

benchConfig = Criterion.Main.defaultConfig
  {
    reportFile = Just "benchmarks.html"
  , timeLimit = 15
  , verbosity = Verbose
  }

main = defaultMainWith benchConfig
  [ env (TIO.readFile "benchmarks/sherlock-holmes.txt") (\file -> bgroup "Sherlock Holmes" [
          bgroup "indent"
            [ bench "indent nothing" $ nf (TW.indent (Just (const False)) ">>>>") file
            , bench "indent normally" $ nf (TW.indent Nothing ">>>>") file
            , bench "indent everything" $ nf (TW.indent (Just (const True)) ">>>>") file
            , bench "indent with Text functions" $ nf (T.unlines . fmap (T.append ">>>>") . T.lines) file
            , bench "indent short lines" $ nf (TW.indent (Just ((<60) . T.length)) ">>>>") file
            ]
        , bench "dedent" $ nf TW.dedent file
        , bgroup "shorten"
            [ bench "shorten to 100" $ nf ((\(Right x) -> x) . TW.shorten TW.defaultConfig{TW.width=100}) file
            , bench "shorten to 1000" $ nf ((\(Right x) -> x) . TW.shorten TW.defaultConfig{TW.width=1000}) file
            , bench "shorten to 10000" $ nf ((\(Right x) -> x) . TW.shorten TW.defaultConfig{TW.width=10000}) file
            , bench "shorten to 100000" $ nf ((\(Right x) -> x) . TW.shorten TW.defaultConfig{TW.width=100000}) file
            ]
        , bgroup "wrap"
          [ bench "wrap default" $ nf ((\(Right x) -> x) . TW.wrap TW.defaultConfig) file
          ]
        , bgroup "fill"
          [ bench "fill default" $ nf ((\(Right x) -> x) . TW.fill TW.defaultConfig) file
          ]
        ])
  ]
