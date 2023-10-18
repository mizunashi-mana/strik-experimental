module Language.Quell.Frontend.Data.Token where

import Language.Quell.Prelude

import qualified Language.Quell.Data.TextId as TextId
import qualified Language.Quell.Data.TextId as Text

type T = Token

data Token
    = TokLexeme LexToken
    | TokWhiteSpace WsToken
    deriving (Eq, Show)

instance Pretty Token where
    pretty = \case
        TokLexeme t ->
            pretty t
        TokWhiteSpace t ->
            pretty t

data LexToken
    = EndOfSource

    | LitPartInterpStringStart Text
    | LitPartInterpStringCont Text
    | LitPartInterpStringEnd Text

    | LitString Text
    | LitRational Rational
    | LitInteger Integer

    | SpBraceOpen
    | SpBraceClose
    | SpBrackOpen
    | SpBrackClose
    | SpParenOpen
    | SpParenClose
    | SpSemi
    | SpDot

    | KwBraceOpen
    | KwBrackOpen
    | KwParenOpen
    | KwUnderscore

    | KwSymEqual
    | KwSymCaret
    | KwSymColon
    | KwSymBackslash

    | IdVarId TextId.T
    | IdConId TextId.T
    | IdVarSym TextId.T
    | IdConSym TextId.T
    | IdFreeId TextId.T
    deriving (Eq, Show)

instance Pretty LexToken where
    pretty = \case
        EndOfSource -> mempty

        -- FIXME: Make to show complete representation.
        -- Current implementation is approximately.
        LitPartInterpStringStart v -> pretty do text "\"" <> v <> text "#{"
        LitPartInterpStringCont v -> pretty do text "#}" <> v <> text "#{"
        LitPartInterpStringEnd v -> pretty do text "#}" <> v <> text "\""
        LitString v -> pretty do text "\"" <> v <> text "\""
        LitInteger v -> pretty v
        LitRational v -> pretty do fromRational @Double v

        SpBraceOpen -> pretty "{"
        SpBraceClose -> pretty "}"
        SpBrackOpen -> pretty "["
        SpBrackClose -> pretty "]"
        SpParenOpen -> pretty "("
        SpParenClose -> pretty ")"
        SpSemi -> pretty ";"
        SpDot -> pretty "."

        KwBraceOpen -> pretty "#{"
        KwBrackOpen -> pretty "#["
        KwParenOpen -> pretty "#("
        KwUnderscore -> pretty "_"

        KwSymBackslash -> pretty "\\"
        KwSymCaret -> pretty "^"
        KwSymColon -> pretty ":"
        KwSymEqual -> pretty "="

        IdVarId v -> pretty v
        IdConId v -> pretty v
        IdVarSym v -> pretty v
        IdConSym v -> pretty v
        -- FIXME: Make to show complete representation.
        -- Current implementation is approximately.
        IdFreeId v -> pretty do text "#\"" <> Text.showByText v <> text "\""

data WsToken
    = CommentLine Text
    | CommentMultiline Text
    deriving (Eq, Show)

instance Pretty WsToken where
    pretty = \case
        CommentLine v -> pretty do text "//" <> v
        CommentMultiline v -> pretty do text "/*" <> v <> text "*/"
