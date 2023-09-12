module Language.Quell.Frontend.Data.Token where

import Language.Quell.Prelude

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

    | KwArrow
    | KwCase
    | KwId
    | KwIf
    | KwIn
    | KwLet
    | KwMatch
    | KwOp
    | KwRec
    | KwWhere
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

        KwArrow -> pretty "#>"
        KwCase -> pretty "#case"
        KwId -> pretty "#id"
        KwIf -> pretty "#if"
        KwIn -> pretty "#in"
        KwLet -> pretty "#let"
        KwMatch -> pretty "#match"
        KwOp -> pretty "#op"
        KwRec -> pretty "#rec"
        KwWhere -> pretty "#where"

data WsToken
    = CommentLine Text
    | CommentMultiline Text
    deriving (Eq, Show)

instance Pretty WsToken where
    pretty = \case
        CommentLine v -> pretty do text "//" <> v
        CommentMultiline v -> pretty do text "/*" <> v <> text "*/"
