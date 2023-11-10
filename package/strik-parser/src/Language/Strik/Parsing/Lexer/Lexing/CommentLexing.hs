{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Lexer.Lexing.CommentLexing where

import           Language.Strik.Prelude

import qualified Language.Strik.Parsing.Lexer.Input               as Input
import qualified Language.Strik.Parsing.Lexer.Lexing.CommentRules as CommentRules
import qualified Language.Strik.Parsing.Spanned                   as Spanned

$(CommentRules.buildLexer)

lexComponentChar :: Spanned.T TextBuilder -> Input.LexItemState (Spanned.T TextBuilder)
lexComponentChar = \s0 -> Input.LexItemState
    { Input.lexItemState = s0
    ,  Input.lexItemNext = go
    }
    where
        go spannedTxtB spannedItem = do
            let (c, _) = Spanned.unSpanned spannedItem
            Input.LexItemState
                {
                    Input.lexItemState = Spanned.Spanned
                        { Spanned.getSpan = Spanned.getSpan spannedTxtB <> Spanned.getSpan spannedItem
                        , Spanned.unSpanned = Spanned.unSpanned spannedTxtB <> textBuilderFromChar c
                        },
                    Input.lexItemNext = go
                }
