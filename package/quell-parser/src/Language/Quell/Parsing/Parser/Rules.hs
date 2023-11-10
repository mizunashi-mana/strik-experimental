{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Parser.Rules where

import Language.Quell.Prelude

import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Frontend.Data.ParsedAst as ParsedAst
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Parser.Ptera.TH                as Ptera
import qualified Language.Haskell.TH as TH
import qualified Language.Quell.Parsing.Parser.AstParsedTag as AstParsedTag
import           Language.Parser.Ptera.TH                (pattern (:*),
                                                          pattern HNil,
                                                          ruleExpr, varA, tokA,
                                                          (<:>), (<^>))
import Language.Quell.Parsing.Parser.LiftTypes ()

type Token = Layout.TokenWithL

type GrammarContext = ()

pattern LexToken :: Token.LexToken -> Layout.TokenWithL
pattern LexToken t <- Layout.TokenRaw (Spanned.Spanned { Spanned.unSpanned = t })

$(Ptera.genGrammarToken
    do TH.mkName "Tokens"
    [t|Token|]
    [ ("EOS",                 [p|LexToken Token.EndOfSource|])

    , ("interp_string_start", [p|LexToken Token.LitPartInterpStringStart{}|])
    , ("interp_string_cont",  [p|LexToken Token.LitPartInterpStringCont{}|])
    , ("interp_string_end",   [p|LexToken Token.LitPartInterpStringEnd{}|])

    , ("string",              [p|LexToken Token.LitString{}|])
    , ("rational",            [p|LexToken Token.LitRational{}|])
    , ("integer",             [p|LexToken Token.LitInteger{}|])

    , ("{",                   [p|LexToken Token.SpBraceOpen{}|])
    , ("}",                   [p|LexToken Token.SpBraceClose{}|])
    , ("[",                   [p|LexToken Token.SpBrackOpen{}|])
    , ("]",                   [p|LexToken Token.SpBrackClose{}|])
    , ("(",                   [p|LexToken Token.SpParenOpen{}|])
    , (")",                   [p|LexToken Token.SpParenClose{}|])
    , (";",                   [p|LexToken Token.SpSemi{}|])
    , (".",                   [p|LexToken Token.SpDot{}|])

    , ("#{",                  [p|LexToken Token.KwBraceOpen{}|])
    , ("#[",                  [p|LexToken Token.KwBrackOpen{}|])
    , ("#(",                  [p|LexToken Token.KwParenOpen{}|])
    , ("_",                   [p|LexToken Token.KwUnderscore{}|])

    , ("=",                   [p|LexToken Token.KwSymEqual{}|])
    , ("^",                   [p|LexToken Token.KwSymCaret{}|])
    , (":",                   [p|LexToken Token.KwSymColon{}|])
    , ("\\",                  [p|LexToken Token.KwSymBackslash{}|])

    , ("var_id",              [p|LexToken Token.IdVarId{}|])
    , ("con_id",              [p|LexToken Token.IdConId{}|])
    , ("var_sym",             [p|LexToken Token.IdVarSym{}|])
    , ("con_sym",             [p|LexToken Token.IdConSym{}|])
    , ("free_id",             [p|LexToken Token.IdFreeId{}|])

    , ("<;>",                 [p|Layout.TokenVirt Layout.VirtSemi|])
    ])

$(Ptera.genRules
    do TH.mkName "RuleDefs"
    do Ptera.GenRulesTypes
        { Ptera.genRulesCtxTy = [t|GrammarContext|]
        , Ptera.genRulesTokensTy = [t|Tokens|]
        , Ptera.genRulesTokenTy = [t|Token|]
        }
    [ (TH.mkName "rdProgramEos", "program EOS", [t|ParsedAst.Program AstParsedTag.T|])
    , (TH.mkName "rdExprEos", "expr EOS", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdTypeEos", "type EOS", [t|ParsedAst.TypeExpr AstParsedTag.T|])

    , (TH.mkName "rdProgram", "program", [t|ParsedAst.Program AstParsedTag.T|])

    , (TH.mkName "rdLocalDecl", "local_decl", [t|ParsedAst.LocalDecl AstParsedTag.T|])
    , (TH.mkName "rdLocalTypeDecl", "local_type_decl", [t|ParsedAst.LocalDecl AstParsedTag.T|])
    , (TH.mkName "rdLetBody", "let_body", [t|Spanned.T [ParsedAst.LetItem AstParsedTag.T]|])
    , (TH.mkName "rdLetBodyItems", "let_body_items", [t|Spanned.T [ParsedAst.LetItem AstParsedTag.T]|])
    , (TH.mkName "rdLetBodyItem", "let_body_item", [t|ParsedAst.LetItem AstParsedTag.T|])
    , (TH.mkName "rdLetTypeBody", "let_type_body", [t|Spanned.T [ParsedAst.LetItem AstParsedTag.T]|])
    , (TH.mkName "rdLetTypeBodyItems", "let_type_body_items", [t|Spanned.T [ParsedAst.LetItem AstParsedTag.T]|])
    , (TH.mkName "rdLetTypeBodyItem", "let_type_body_item", [t|ParsedAst.LetItem AstParsedTag.T|])

    , (TH.mkName "rdWhereBody", "where_body", [t|Spanned.T [ParsedAst.Decl AstParsedTag.T]|])
    , (TH.mkName "rdWhereBodyItems", "where_body_items", [t|Spanned.T [ParsedAst.Decl AstParsedTag.T]|])
    , (TH.mkName "rdWhereBodyItem", "where_body_item", [t|ParsedAst.Decl AstParsedTag.T|])

    , (TH.mkName "rdBindExpr", "bind_expr", [t|ParsedAst.Decl AstParsedTag.T|])
    , (TH.mkName "rdBindType", "bind_type", [t|ParsedAst.Decl AstParsedTag.T|])
    , (TH.mkName "rdBindPromType", "bind_prom_type", [t|ParsedAst.Decl AstParsedTag.T|])

    , (TH.mkName "rdExpr", "expr", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprAnn", "expr_ann", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprInfix", "expr_infix", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprOp", "expr_op", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprApps", "expr_apps", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprBlock", "expr_block", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprAtomic", "expr_atomic", [t|ParsedAst.Expr AstParsedTag.T|])
    , (TH.mkName "rdExprLiteral", "expr_literal", [t|ParsedAst.Expr AstParsedTag.T|])

    , (TH.mkName "rdCaseBody", "case_body", [t|Spanned.T [ParsedAst.CaseItem AstParsedTag.T]|])
    , (TH.mkName "rdCaseItems", "case_items", [t|Spanned.T [ParsedAst.CaseItem AstParsedTag.T]|])
    , (TH.mkName "rdCaseItem", "case_item", [t|ParsedAst.CaseItem AstParsedTag.T|])

    , (TH.mkName "rdBlock", "block", [t|ParsedAst.Block AstParsedTag.T|])
    , (TH.mkName "rdBlockItems", "block_items", [t|Spanned.T [ParsedAst.BlockItem AstParsedTag.T]|])
    , (TH.mkName "rdBlockItem", "block_item", [t|ParsedAst.BlockItem AstParsedTag.T|])
    , (TH.mkName "rdBlockPats", "block_pats", [t|Spanned.T [ParsedAst.Pat AstParsedTag.T]|])
    , (TH.mkName "rdBlockGuard", "block_guard", [t|Spanned.T (ParsedAst.View AstParsedTag.T)|])
    , (TH.mkName "rdBlockStmts", "block_stmts", [t|Spanned.T [ParsedAst.BlockStmt AstParsedTag.T]|])
    , (TH.mkName "rdBlockStmt", "block_stmt", [t|ParsedAst.BlockStmt AstParsedTag.T|])

    , (TH.mkName "rdExprInterpString", "expr_interp_string", [t|ParsedAst.Expr AstParsedTag.T|])

    , (TH.mkName "rdExprTuple", "expr_tuple", [t|ParsedAst.Tuple AstParsedTag.T|])
    , (TH.mkName "rdExprTupleItems", "expr_tuple_items", [t|Spanned.T [ParsedAst.TupleItem AstParsedTag.T]|])
    , (TH.mkName "rdExprTupleItem", "expr_tuple_item", [t|ParsedAst.TupleItem AstParsedTag.T|])

    , (TH.mkName "rdType", "type", [t|ParsedAst.TypeExpr AstParsedTag.T|])
    ])

$(Ptera.genParsePoints
    do TH.mkName "ParsePoints"
    do TH.mkName "RuleDefs"
    [ "program EOS"
    , "type EOS"
    , "expr EOS"
    ])

type RuleExpr = Ptera.RuleExprM GrammarContext RuleDefs Tokens Token
type Alt = Ptera.AltM GrammarContext RuleDefs Tokens Token
type TypedExpr = Ptera.TypedExpr RuleDefs Tokens Token
type SemAct = Ptera.SemActM GrammarContext
type ActionTask = Ptera.ActionTask GrammarContext

grammar :: Ptera.GrammarM GrammarContext RuleDefs Tokens Token ParsePoints
grammar = Ptera.fixGrammar
    do RuleDefs
        {
            rdProgramEos = rProgramEos,
            rdExprEos = rExprEos,
            rdTypeEos = rTypeEos,

            rdProgram = rProgram,

            rdLocalDecl = rLocalDecl,
            rdLocalTypeDecl = rLocalTypeDecl,
            rdLetBody = rLetBody,
            rdLetBodyItems = rLetBodyItems,
            rdLetBodyItem = rLetBodyItem,
            rdLetTypeBody = rLetTypeBody,
            rdLetTypeBodyItems = rLetTypeBodyItems,
            rdLetTypeBodyItem = rLetTypeBodyItem,

            rdWhereBody = rWhereBody,
            rdWhereBodyItems = rWhereBodyItems,
            rdWhereBodyItem = rWhereBodyItem,

            rdBindExpr = rBindExpr,
            rdBindType = rBindType,
            rdBindPromType = rBindPromType,

            rdExpr = rExpr,
            rdExprAnn = rExprAnn,
            rdExprInfix = rExprInfix,
            rdExprOp = rExprOp,
            rdExprApps = rExprApps,
            rdExprBlock = rExprBlock,
            rdExprAtomic = rExprAtomic,
            rdExprLiteral = rExprLiteral,

            rdCaseBody = rCaseBody,
            rdCaseItems = rCaseItems,
            rdCaseItem = rCaseItem,

            rdBlock = rBlock,
            rdBlockItems = rBlockItems,
            rdBlockItem = rBlockItem,
            rdBlockPats = rBlockPats,
            rdBlockGuard = rBlockGuard,
            rdBlockStmts = rBlockStmts,
            rdBlockStmt = rBlockStmt,

            rdExprInterpString = rExprInterpString,

            rdExprTuple = rExprTuple,
            rdExprTupleItems = rExprTupleItems,
            rdExprTupleItem = rExprTupleItem,

            rdType = rType
        }


rProgramEos :: RuleExpr (ParsedAst.Program AstParsedTag.T)
rProgramEos = ruleExpr
    [ varA @"program" <^> tokA @"EOS"
        <:> \(program :* _ :* HNil) ->
            program
    ]

rExprEos :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprEos = ruleExpr
    [ varA @"expr" <^> tokA @"EOS"
        <:> \(expr :* _ :* HNil) ->
            expr
    ]

rTypeEos :: RuleExpr (ParsedAst.TypeExpr AstParsedTag.T)
rTypeEos = ruleExpr
    [ varA @"type" <^> tokA @"EOS"
        <:> \(ty :* _ :* HNil) ->
            ty
    ]


rProgram :: RuleExpr (ParsedAst.Program AstParsedTag.T)
rProgram = ruleExpr
    [ varA @"expr"
        <:> \(e :* HNil) ->
            [||ParsedAst.Program
                { ParsedAst.expr = $$(e)
                , ParsedAst.extra = undefined
                }
            ||]
    ]


rLocalDecl :: RuleExpr (ParsedAst.LocalDecl AstParsedTag.T)
rLocalDecl = undefined

rLocalTypeDecl :: RuleExpr (ParsedAst.LocalDecl AstParsedTag.T)
rLocalTypeDecl = undefined

rLetBody :: RuleExpr (Spanned.T [ParsedAst.LetItem AstParsedTag.T])
rLetBody = undefined

rLetBodyItems :: RuleExpr (Spanned.T [ParsedAst.LetItem AstParsedTag.T])
rLetBodyItems = undefined

rLetBodyItem :: RuleExpr (ParsedAst.LetItem AstParsedTag.T)
rLetBodyItem = undefined

rLetTypeBody :: RuleExpr (Spanned.T [ParsedAst.LetItem AstParsedTag.T])
rLetTypeBody = undefined

rLetTypeBodyItems :: RuleExpr (Spanned.T [ParsedAst.LetItem AstParsedTag.T])
rLetTypeBodyItems = undefined

rLetTypeBodyItem :: RuleExpr (ParsedAst.LetItem AstParsedTag.T)
rLetTypeBodyItem = undefined


rWhereBody :: RuleExpr (Spanned.T [ParsedAst.Decl AstParsedTag.T])
rWhereBody = undefined

rWhereBodyItems :: RuleExpr (Spanned.T [ParsedAst.Decl AstParsedTag.T])
rWhereBodyItems = undefined

rWhereBodyItem :: RuleExpr (ParsedAst.Decl AstParsedTag.T)
rWhereBodyItem = undefined


rBindExpr :: RuleExpr (ParsedAst.Decl AstParsedTag.T)
rBindExpr = undefined

rBindType :: RuleExpr (ParsedAst.Decl AstParsedTag.T)
rBindType = undefined

rBindPromType :: RuleExpr (ParsedAst.Decl AstParsedTag.T)
rBindPromType = undefined


rExpr :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExpr = undefined

rExprAnn :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprAnn = undefined

rExprInfix :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprInfix = undefined

rExprOp :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprOp = undefined

rExprApps :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprApps = undefined

rExprBlock :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprBlock = undefined

rExprAtomic :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprAtomic = undefined

rExprLiteral :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprLiteral = undefined


rCaseBody :: RuleExpr (Spanned.T [ParsedAst.CaseItem AstParsedTag.T])
rCaseBody = undefined

rCaseItems :: RuleExpr (Spanned.T [ParsedAst.CaseItem AstParsedTag.T])
rCaseItems = undefined

rCaseItem :: RuleExpr (ParsedAst.CaseItem AstParsedTag.T)
rCaseItem = undefined


rBlock :: RuleExpr (ParsedAst.Block AstParsedTag.T)
rBlock = undefined

rBlockItems :: RuleExpr (Spanned.T [ParsedAst.BlockItem AstParsedTag.T])
rBlockItems = undefined

rBlockItem :: RuleExpr (ParsedAst.BlockItem AstParsedTag.T)
rBlockItem = undefined

rBlockPats :: RuleExpr (Spanned.T [ParsedAst.Pat AstParsedTag.T])
rBlockPats = undefined

rBlockGuard :: RuleExpr (Spanned.T (ParsedAst.View AstParsedTag.T))
rBlockGuard = undefined

rBlockStmts :: RuleExpr (Spanned.T [ParsedAst.BlockStmt AstParsedTag.T])
rBlockStmts = undefined

rBlockStmt :: RuleExpr (ParsedAst.BlockStmt AstParsedTag.T)
rBlockStmt = undefined


rExprInterpString :: RuleExpr (ParsedAst.Expr AstParsedTag.T)
rExprInterpString = undefined


rExprTuple :: RuleExpr (ParsedAst.Tuple AstParsedTag.T)
rExprTuple = undefined

rExprTupleItems :: RuleExpr (Spanned.T [ParsedAst.TupleItem AstParsedTag.T])
rExprTupleItems = undefined

rExprTupleItem :: RuleExpr (ParsedAst.TupleItem AstParsedTag.T)
rExprTupleItem = undefined


rType :: RuleExpr (ParsedAst.TypeExpr AstParsedTag.T)
rType = undefined
