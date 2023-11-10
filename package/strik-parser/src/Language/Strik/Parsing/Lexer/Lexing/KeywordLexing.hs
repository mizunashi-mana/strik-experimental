{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Lexer.Lexing.KeywordLexing where

import           Language.Strik.Prelude

import qualified Language.Lexer.Tlex                              as Tlex
import qualified Language.Strik.Data.Monad.MonadST                as MonadST
import qualified Language.Strik.Frontend.Data.Token               as Token
import qualified Language.Strik.Parsing.Lexer.Input               as Input
import qualified Language.Strik.Parsing.Lexer.Lexing.KeywordRules as KeywordRules

$(KeywordRules.buildLexer)

lexer :: forall s m. MonadST.T s m => Int -> Input.Lexer s m (Maybe Token.LexToken)
lexer expectedEnd = tlexScan KeywordRules.Initial >>= \case
    Tlex.TlexEndOfInput -> do
        pure Nothing
    Tlex.TlexNotAccepted -> do
        pure Nothing
    Tlex.TlexAccepted endPos tok -> if
        | expectedEnd == endPos -> pure Nothing
        | otherwise             -> pure do Just tok
