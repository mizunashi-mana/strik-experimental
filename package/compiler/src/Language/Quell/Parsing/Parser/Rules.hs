{-# LANGUAGE TemplateHaskell       #-}

module Language.Quell.Parsing.Parser.Rules where

import qualified Language.Haskell.TH              as TH
import           Language.Parser.Ptera.TH         (varA)
import qualified Language.Parser.Ptera.TH         as Ptera
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Parsing.Parser.Layout as Layout


$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Token.T|]
    [ ("EOS",           [p|Token.EndOfSource|])

    , ("#as",           [p|Token.KwAs|])
    , ("#case",         [p|Token.KwCase|])
    , ("#data",         [p|Token.KwData|])
    , ("#default",      [p|Token.KwDefault|])
    , ("#derive",       [p|Token.KwDerive|])
    , ("#do",           [p|Token.KwDo|])
    , ("#export",       [p|Token.KwExport|])
    , ("#family",       [p|Token.KwFamily|])
    , ("#foreign",      [p|Token.KwForeign|])
    , ("#impl",         [p|Token.KwImpl|])
    , ("#in",           [p|Token.KwIn|])
    , ("#infix",        [p|Token.KwInfix|])
    , ("#let",          [p|Token.KwLet|])
    , ("#letrec",       [p|Token.KwLetrec|])
    , ("#match",        [p|Token.KwMatch|])
    , ("#module",       [p|Token.KwModule|])
    , ("#newtype",      [p|Token.KwNewtype|])
    , ("#pattern",      [p|Token.KwPattern|])
    , ("#rec",          [p|Token.KwRec|])
    , ("#record",       [p|Token.KwRecord|])
    , ("#role",         [p|Token.KwRole|])
    , ("#self",         [p|Token.KwSelf|])
    , ("#sig",          [p|Token.KwSignature|])
    , ("#static",       [p|Token.KwStatic|])
    , ("#trait",        [p|Token.KwTrait|])
    , ("#type",         [p|Token.KwType|])
    , ("#use",          [p|Token.KwUse|])
    , ("#with",         [p|Token.KwWith|])
    , ("#when",         [p|Token.KwWhen|])
    , ("#where",        [p|Token.KwWhere|])
    , ("#yield",        [p|Token.KwYield|])

    , ("#Default",      [p|Token.LKwDefault|])
    , ("#Self",         [p|Token.LKwSelf|])

    , ("->",            [p|Token.SymArrow|])
    , ("@",             [p|Token.SymAt|])
    , ("!",             [p|Token.SymBang|])
    , (":",             [p|Token.SymColon|])
    , ("=>",            [p|Token.SymDArrow|])
    , ("<=",            [p|Token.SymDLeftArrow|])
    , ("=",             [p|Token.SymEqual|])
    , ("^",             [p|Token.SymForall|])
    , ("\\",            [p|Token.SymLambda|])
    , ("<-",            [p|Token.SymLeftArrow|])
    , ("|",             [p|Token.SymOr|])
    , ("~",             [p|Token.SymTilde|])
    , ("_",             [p|Token.SymUnderscore|])
    , ("?",             [p|Token.SymUnknown|])

    , ("`",             [p|Token.SpBackquote|])
    , ("##",            [p|Token.SpBlock|])
    , ("[",             [p|Token.SpBrackOpen|])
    , ("]",             [p|Token.SpBrackClose|])
    , (",",             [p|Token.SpComma|])
    , ("{",             [p|Token.SpBraceOpen|])
    , ("}",             [p|Token.SpBraceClose|])
    , ("{{",            [p|Token.SpDBraceOpen|])
    , ("}}",            [p|Token.SpDBraceClose|])
    , (".",             [p|Token.SpDot|])
    , ("..",            [p|Token.SpDots|])
    , ("(",             [p|Token.SpParenOpen|])
    , (")",             [p|Token.SpParenClose|])
    , (";",             [p|Token.SpSemi|])
    , ("#>",            [p|Token.SpThen|])
    , ("#@",            [p|Token.SpTypeBlock|])
    , ("v{",            [p|Token.SpVBraceOpen|])
    , ("v}",            [p|Token.SpVBraceClose|])
    , ("v;",            [p|Token.SpVSemi|])

    , ("con_id",        [p|Token.IdConId{}|])
    , ("con_sym",       [p|Token.IdConSym{}|])
    , ("var_id",        [p|Token.IdVarId{}|])
    , ("var_sym",       [p|Token.IdVarSym{}|])

    , ("bytechar",      [p|Token.LitByteChar{}|])
    , ("bytestring",    [p|Token.LitByteString{}|])
    , ("integer",       [p|Token.LitInteger{}|])
    , ("rational",      [p|Token.LitRational{}|])
    , ("char",          [p|Token.LitChar{}|])
    , ("string",        [p|Token.LitString{}|])

    , ("interp_string_without_interp",
                        [p|Token.LitInterpStringWithoutInterp{}|])
    , ("interp_string_start",
                        [p|Token.LitInterpStringStart{}|])
    , ("interp_string_cont",
                        [p|Token.LitInterpStringCont{}|])
    , ("interp_string_end",
                        [p|Token.LitInterpStringEnd{}|])
    ])

data GrammarContext = GrammarContext
    { layoutStack :: [LayoutItem]
    }
    deriving (Eq, Show)

data LayoutItem
    = NoLayout
    | ImplicitLayout Layout.LayoutPos
    | ExplicitScopedLayout Layout.LayoutPos
    deriving (Eq, Show)
