{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing.NumberLexing where

import qualified Language.Quell.Parsing.Lexer.Lexing.NumberRules as NumberRules

$(NumberRules.buildLexer)
