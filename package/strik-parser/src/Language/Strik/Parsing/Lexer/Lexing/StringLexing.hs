{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Lexer.Lexing.StringLexing where

import           Language.Strik.Prelude

import qualified Language.Strik.Parsing.Lexer.Input              as Input
import qualified Language.Strik.Parsing.Lexer.Lexing.StringRules as StringRules
import qualified Language.Strik.Parsing.Spanned                  as Spanned

$(StringRules.buildLexer)

lexComponentChar :: Input.LexItemState TextBuilder
lexComponentChar = Input.LexItemState
    {
        Input.lexItemState = mempty,
        Input.lexItemNext = go
    }
    where
        go txtB spannedItem = do
            let (c, _) = Spanned.unSpanned spannedItem
            Input.LexItemState
                {
                    Input.lexItemState = txtB <> textBuilderFromChar c,
                    Input.lexItemNext = go
                }
