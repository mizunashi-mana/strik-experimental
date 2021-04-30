module Language.Quell.Parsing.Parser.Error (
    T,
    Error (..),
) where

import           Language.Quell.Prelude


type T = Error

data Error
    = UnexpectedExtConInDecl
    | UnexpectedExtConopInDecl
    | UnexpectedExtVarInDecl
    | UnexpectedExtOpInDecl
    | ExpectedDBraceClose
    | ExpectedBraceClose
    | ExpectedBrackClose
    | ExpectedParenClose
    | ExpectedInterpStringClose
    | NotOpenedDBraceClose
    | NotOpenedBraceClose
    | NotOpenedBrackClose
    | NotOpenedParenClose
    | NotOpenedInterpStringClose
    | ParseError -- FIXME
    deriving (Eq, Show)
