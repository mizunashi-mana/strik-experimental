module Language.Quell.Type.Token (
  T,
  Token (..),
  LexToken (..),
  WsToken (..),
) where

import           Language.Quell.Prelude

import qualified GHC.Show                   as GHC
import qualified Language.Quell.Type.TextId as TextId
import qualified Prelude


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

    | KwAs
    | KwCase
    | KwData
    | KwDefault
    | KwDerive
    | KwDo
    | KwExport
    | KwFamily
    | KwForeign
    | KwImpl
    | KwIn
    | KwInfix
    | KwLet
    | KwLetrec
    | KwMatch
    | KwModule
    | KwNewtype
    | KwPattern
    | KwRec
    | KwRecord
    | KwRole
    | KwSelf
    | KwSignature
    | KwStatic
    | KwTrait
    | KwType
    | KwUse
    | KwWith
    | KwWhen
    | KwWhere
    | KwYield

    | LKwDefault
    | LKwSelf

    | SymAt
    | SymBang
    | SymColon
    | SymEqual
    | SymForall
    | SymLambda
    | SymOr
    | SymTilde
    | SymUnderscore
    | SymUnknown

    | SpBackquote
    | SpBind
    | SpBlock
    | SpBrackOpen
    | SpBrackClose
    | SpComma
    | SpBraceOpen
    | SpBraceClose
    | SpDBraceOpen
    | SpDBraceClose
    | SpDot
    | SpDots
    | SpParenOpen
    | SpParenClose
    | SpSemi
    | SpThen
    | SpTypeBlock

    | IdConId TextId.T
    | IdConSym TextId.T
    | IdVarId TextId.T
    | IdVarSym TextId.T

    | LitByteChar Word8
    | LitByteString ByteString
    | LitInteger Integer
    | LitRational Rational
    | LitChar Char
    | LitString Text

    | InterpStringWithoutInterp Text
    | InterpStringStart Text
    | InterpStringContinue Text
    | InterpStringEnd Text
    deriving (Eq, Show)

data WsToken
    = CommentLine Text
    | CommentMultiline Text
    | CommentPragma Text
    | CommentDoc Text
    deriving (Eq, Show)

instance Pretty LexToken where
    pretty = \case
        EndOfSource                     -> mempty
        KwAs                            -> pretty "#as"
        KwCase                          -> pretty "#case"
        KwData                          -> pretty "#data"
        KwDefault                       -> pretty "#default"
        KwDerive                        -> pretty "#derive"
        KwDo                            -> pretty "#do"
        KwExport                        -> pretty "#export"
        KwFamily                        -> pretty "#family"
        KwForeign                       -> pretty "#foreign"
        KwImpl                          -> pretty "#impl"
        KwIn                            -> pretty "#in"
        KwInfix                         -> pretty "#infix"
        KwLet                           -> pretty "#let"
        KwLetrec                        -> pretty "#letrec"
        KwMatch                         -> pretty "#match"
        KwModule                        -> pretty "#mod"
        KwNewtype                       -> pretty "#newtype"
        KwPattern                       -> pretty "#pattern"
        KwRec                           -> pretty "#rec"
        KwRecord                        -> pretty "#record"
        KwRole                          -> pretty "#role"
        KwSelf                          -> pretty "#self"
        KwSignature                     -> pretty "#sig"
        KwStatic                        -> pretty "#static"
        KwTrait                         -> pretty "#trait"
        KwType                          -> pretty "#type"
        KwUse                           -> pretty "#use"
        KwWith                          -> pretty "#with"
        KwWhen                          -> pretty "#when"
        KwWhere                         -> pretty "#where"
        KwYield                         -> pretty "#yield"
        LKwDefault                      -> pretty "#Default"
        LKwSelf                         -> pretty "#Self"
        SymAt                           -> pretty "@"
        SymBang                         -> pretty "!"
        SymColon                        -> pretty ":"
        SymEqual                        -> pretty "="
        SymForall                       -> pretty "^"
        SymLambda                       -> pretty "\\"
        SymOr                           -> pretty "|"
        SymTilde                        -> pretty "~"
        SymUnderscore                   -> pretty "_"
        SymUnknown                      -> pretty "?"
        SpBackquote                     -> pretty "`"
        SpBrackOpen                     -> pretty "["
        SpBrackClose                    -> pretty "]"
        SpComma                         -> pretty ","
        SpBind                          -> pretty "#<"
        SpBraceOpen                     -> pretty "{"
        SpBraceClose                    -> pretty "}"
        SpDBraceOpen                    -> pretty "{{"
        SpDBraceClose                   -> pretty "}}"
        SpDot                           -> pretty "."
        SpDots                          -> pretty ".."
        SpParenOpen                     -> pretty "("
        SpParenClose                    -> pretty ")"
        SpSemi                          -> pretty ";"
        SpBlock                         -> pretty "##"
        SpThen                          -> pretty "#>"
        SpTypeBlock                     -> pretty "#@"
        IdConId v                       -> pretty v
        IdConSym v                      -> pretty v
        IdVarId v                       -> pretty v
        IdVarSym v                      -> pretty v
        LitByteChar v                   -> prettyByteChar v
        LitByteString v                 -> prettyByteString v
        LitChar v                       -> pretty do text "'" <> opoint v <> text "'" -- FIXME: escape
        LitString v                     -> pretty do text "\"" <> v <> text "\"" -- FIXME: escape
        LitInteger v                    -> pretty v
        LitRational v                   -> prettyRational v
        InterpStringWithoutInterp v     -> pretty do text "#s\"" <> v <> text "\"" -- FIXME: escape
        InterpStringStart v             -> pretty do text "#s\"" <> v <> text "${#" -- FIXME: escape
        InterpStringContinue v          -> pretty do text "#}" <> v <> text "${#" -- FIXME: escape
        InterpStringEnd v               -> pretty do text "#}" <> v <> text "\"" -- FIXME: escape

instance Pretty WsToken where
    pretty = \case
        CommentLine v      -> pretty do text "--" <> v
        CommentMultiline v -> pretty do text "{-" <> v <> text "-}"
        CommentPragma v    -> pretty do text "{-#" <> v <> text "#-}"
        CommentDoc v       -> pretty do text "{-!" <> v <> text "\n|-}"

-- FIXME: Current implementation is approximately.
-- Make to show complete representation.
prettyRational :: Rational -> Doc ann
prettyRational v = pretty do fromRational @Double v

prettyByteChar :: Word8 -> Doc ann
prettyByteChar w = if
    | w <= 0x7F ->
        let c = toEnum @Char do fromInteger @Int do toInteger w
        in pretty do "#" <> show c
    | otherwise -> pretty do "#'\\x" <> showByteHex w <> "'"

prettyByteString :: ByteString -> Doc ann
prettyByteString bs = pretty do "#\"" <> bsShow <> "\"" where
    bsShow = ofoldMap
        do \w -> if
            | w <= 0x7F ->
                let c = toEnum do fromIntegral w
                in GHC.showLitChar c ""
            | otherwise -> "\\x" <> showByteHex w
        bs

showByteHex :: Word8 -> Prelude.String
showByteHex = \w -> if
    | w <= 0xF  -> "0" <> showByteHex1Digit w
    | otherwise -> showByteHex1Digit (w `div` 0x10) <> showByteHex1Digit (w `mod` 0x10)
    where
        showByteHex1Digit w = if
            | w <= 0x9  -> show w
            | otherwise -> [toEnum do fromEnum 'A' - 0xA + fromIntegral w]
