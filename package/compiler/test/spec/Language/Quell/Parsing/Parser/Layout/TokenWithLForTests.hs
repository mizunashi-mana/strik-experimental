module Language.Quell.Parsing.Parser.Layout.TokenWithLForTests (
    TokenWithLForTests (..),
    fromTokenWithL,
) where

import Language.Quell.Prelude

import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Parsing.Spanned as Spanned

data TokenWithLForTests
    = Token Token.LexToken
    | ExpectNewImplicitLayout Layout.Position
    | Newline Layout.Position
    deriving (Eq, Show)

fromTokenWithL :: Layout.TokenWithL -> TokenWithLForTests
fromTokenWithL = \case
    Layout.Token st ->
        Token do Spanned.unSpanned st
    Layout.ExpectNewImplicitLayout p ->
        ExpectNewImplicitLayout p
    Layout.Newline p ->
        Newline p
