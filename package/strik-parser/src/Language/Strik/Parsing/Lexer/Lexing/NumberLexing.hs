{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Lexer.Lexing.NumberLexing where

import qualified Language.Strik.Parsing.Lexer.Lexing.NumberRules as NumberRules

$(NumberRules.buildLexer)
