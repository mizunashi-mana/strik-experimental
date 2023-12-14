{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Parser.Rules where

import           Language.Strik.Prelude

import qualified Language.Haskell.TH                        as TH
import           Language.Parser.Ptera.TH                   (eps, pattern (:*),
                                                             pattern HNil,
                                                             ruleExpr, tokA,
                                                             varA, (<:>), (<^>))
import qualified Language.Parser.Ptera.TH                   as Ptera
import           Language.Strik.Frontend.Data.ParsedAst     (extractX)
import qualified Language.Strik.Frontend.Data.ParsedAst     as ParsedAst
import qualified Language.Strik.Frontend.Data.Token         as Token
import           Language.Strik.Parsing.Parser.AstParsedTag (AstParsedTag,
                                                             tokenSpan, (<>>))
import qualified Language.Strik.Parsing.Parser.Layout       as Layout
import           Language.Strik.Parsing.Parser.LiftTypes    ()
import           Language.Strik.Parsing.Spanned             (Span, Spanned (..))

type Token = Layout.TokenWithL

type GrammarContext = ()

pattern LexToken :: Token.LexToken -> Layout.TokenWithL
pattern LexToken t <- Layout.TokenRaw (Spanned { unSpanned = t })

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

    , ("#let",                [p|LexToken Token.KwLet{}|])
    , ("#rec",                [p|LexToken Token.KwRec{}|])

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
    [ (TH.mkName "rdProgramEos", "program EOS", [t|ParsedAst.Program AstParsedTag|])
    , (TH.mkName "rdExprEos", "expr EOS", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdTypeEos", "type EOS", [t|ParsedAst.TypeExpr AstParsedTag|])

    , (TH.mkName "rdProgram", "program", [t|ParsedAst.Program AstParsedTag|])

    , (TH.mkName "rdLocalDecl", "local_decl", [t|ParsedAst.LocalDecl AstParsedTag|])
    , (TH.mkName "rdLocalTypeDecl", "local_type_decl", [t|ParsedAst.LocalDecl AstParsedTag|])
    , (TH.mkName "rdLetBody", "let_body", [t|Spanned [ParsedAst.LetItem AstParsedTag]|])
    , (TH.mkName "rdLetBodyItems", "let_body_items", [t|Spanned [ParsedAst.LetItem AstParsedTag]|])
    , (TH.mkName "rdLetBodyItem", "let_body_item", [t|ParsedAst.LetItem AstParsedTag|])
    , (TH.mkName "rdLetTypeBody", "let_type_body", [t|Spanned [ParsedAst.LetItem AstParsedTag]|])
    , (TH.mkName "rdLetTypeBodyItems", "let_type_body_items", [t|Spanned [ParsedAst.LetItem AstParsedTag]|])
    , (TH.mkName "rdLetTypeBodyItem", "let_type_body_item", [t|ParsedAst.LetItem AstParsedTag|])

    , (TH.mkName "rdWhereBody", "where_body", [t|Spanned [ParsedAst.Decl AstParsedTag]|])
    , (TH.mkName "rdWhereBodyItems", "where_body_items", [t|Spanned [ParsedAst.Decl AstParsedTag]|])
    , (TH.mkName "rdWhereBodyItem", "where_body_item", [t|ParsedAst.Decl AstParsedTag|])

    , (TH.mkName "rdBindExpr", "bind_expr", [t|ParsedAst.Decl AstParsedTag|])
    , (TH.mkName "rdBindType", "bind_type", [t|ParsedAst.Decl AstParsedTag|])
    , (TH.mkName "rdBindPromType", "bind_prom_type", [t|ParsedAst.Decl AstParsedTag|])

    , (TH.mkName "rdExpr", "expr", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprAnn", "expr_ann", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprInfix", "expr_infix", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprOp", "expr_op", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprApps", "expr_apps", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprBlock", "expr_block", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprAtomic", "expr_atomic", [t|ParsedAst.Expr AstParsedTag|])
    , (TH.mkName "rdExprLiteral", "expr_literal", [t|ParsedAst.Expr AstParsedTag|])

    , (TH.mkName "rdCaseBody", "case_body", [t|Spanned [ParsedAst.CaseItem AstParsedTag]|])
    , (TH.mkName "rdCaseItems", "case_items", [t|Spanned [ParsedAst.CaseItem AstParsedTag]|])
    , (TH.mkName "rdCaseItem", "case_item", [t|ParsedAst.CaseItem AstParsedTag|])

    , (TH.mkName "rdBlock", "block", [t|ParsedAst.Block AstParsedTag|])
    , (TH.mkName "rdBlockItems", "block_items", [t|Spanned [ParsedAst.BlockItem AstParsedTag]|])
    , (TH.mkName "rdBlockItem", "block_item", [t|ParsedAst.BlockItem AstParsedTag|])
    , (TH.mkName "rdBlockPats", "block_pats", [t|Spanned [ParsedAst.Pat AstParsedTag]|])
    , (TH.mkName "rdBlockGuard", "block_guard", [t|Spanned (ParsedAst.View AstParsedTag)|])
    , (TH.mkName "rdBlockStmts", "block_stmts", [t|Spanned [ParsedAst.BlockStmt AstParsedTag]|])
    , (TH.mkName "rdBlockStmt", "block_stmt", [t|ParsedAst.BlockStmt AstParsedTag|])

    , (TH.mkName "rdExprInterpString", "expr_interp_string", [t|ParsedAst.Expr AstParsedTag|])

    , (TH.mkName "rdExprTuple", "expr_tuple", [t|ParsedAst.Tuple AstParsedTag|])
    , (TH.mkName "rdExprTupleItems", "expr_tuple_items", [t|Spanned [ParsedAst.TupleItem AstParsedTag]|])
    , (TH.mkName "rdExprTupleItem", "expr_tuple_item", [t|ParsedAst.TupleItem AstParsedTag|])

    , (TH.mkName "rdType", "type", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeAnn", "type_ann", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeInfix", "type_infix", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeOp", "type_op", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeApps", "type_apps", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeAtomic", "type_atomic", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeLiteral", "type_literal", [t|ParsedAst.TypeExpr AstParsedTag|])

    , (TH.mkName "rdBlockType", "block_type", [t|ParsedAst.Block AstParsedTag|])
    , (TH.mkName "rdBlockTypeStmts", "block_type_stmts", [t|Spanned [ParsedAst.BlockStmt AstParsedTag]|])
    , (TH.mkName "rdBlockTypeStmt", "block_type_stmt", [t|ParsedAst.BlockStmt AstParsedTag|])

    , (TH.mkName "rdTypeTuple", "type_tuple", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeTupleItems", "type_tuple_items", [t|Spanned [ParsedAst.TypeTupleItem AstParsedTag]|])
    , (TH.mkName "rdTypeTupleItem", "type_tuple_item", [t|ParsedAst.TypeTupleItem AstParsedTag|])

    , (TH.mkName "rdTypeSigTuple", "type_sig_tuple", [t|ParsedAst.TypeExpr AstParsedTag|])
    , (TH.mkName "rdTypeSigTupleItems", "type_sig_tuple_items", [t|Spanned [ParsedAst.TypeTupleItem AstParsedTag]|])
    , (TH.mkName "rdTypeSigTupleItem", "type_sig_tuple_item", [t|ParsedAst.TypeTupleItem AstParsedTag|])

    , (TH.mkName "rdLbOpen", "lb_open", [t|Span|])
    , (TH.mkName "rdLbImpOpen", "lb_imp_open", [t|Span|])
    , (TH.mkName "rdLbExpOpen", "lb_exp_open", [t|Span|])
    , (TH.mkName "rdLbClose", "lb_close", [t|Span|])
    , (TH.mkName "rdLpOpen", "lp_open", [t|Span|])
    , (TH.mkName "rdLpImpOpen", "lp_imp_open", [t|Span|])
    , (TH.mkName "rdLpExpOpen", "lp_exp_open", [t|Span|])
    , (TH.mkName "rdLpClose", "lp_close", [t|Span|])
    , (TH.mkName "rdLsemis", "lsemis", [t|Maybe Span|])
    , (TH.mkName "rdMayLsemis", "lsemis?", [t|Maybe Span|])
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

            rdType = rType,
            rdTypeAnn = rTypeAnn,
            rdTypeInfix = rTypeInfix,
            rdTypeOp = rTypeOp,
            rdTypeApps = rTypeApps,
            rdTypeAtomic = rTypeAtomic,
            rdTypeLiteral = rTypeLiteral,

            rdBlockType = rBlockType,
            rdBlockTypeStmts = rBlockTypeStmts,
            rdBlockTypeStmt = rBlockTypeStmt,

            rdTypeTuple = rTypeTuple,
            rdTypeTupleItems = rTypeTupleItems,
            rdTypeTupleItem = rTypeTupleItem,

            rdTypeSigTuple = rTypeSigTuple,
            rdTypeSigTupleItems = rTypeSigTupleItems,
            rdTypeSigTupleItem = rTypeSigTupleItem,

            rdLbOpen = rLbOpen,
            rdLbImpOpen = rLbImpOpen,
            rdLbExpOpen = rLbExpOpen,
            rdLbClose = rLbClose,
            rdLpOpen = rLpOpen,
            rdLpImpOpen = rLpImpOpen,
            rdLpExpOpen = rLpExpOpen,
            rdLpClose = rLpClose,
            rdLsemis = rLsemis,
            rdMayLsemis = rMayLsemis
        }


rProgramEos :: RuleExpr (ParsedAst.Program AstParsedTag)
rProgramEos = ruleExpr
    [ varA @"program" <^> tokA @"EOS"
        <:> \(program :* _ :* HNil) ->
            program
    ]

rExprEos :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprEos = ruleExpr
    [ varA @"expr" <^> tokA @"EOS"
        <:> \(expr :* _ :* HNil) ->
            expr
    ]

rTypeEos :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeEos = ruleExpr
    [ varA @"type" <^> tokA @"EOS"
        <:> \(ty :* _ :* HNil) ->
            ty
    ]


rProgram :: RuleExpr (ParsedAst.Program AstParsedTag)
rProgram = ruleExpr
    [ varA @"expr"
        <:> \(e :* HNil) ->
            [||ParsedAst.Program
                { ParsedAst.expr = $$(e)
                , ParsedAst.extra = extractX $$(e)
                }
            ||]
    ]


rLocalDecl :: RuleExpr (ParsedAst.LocalDecl AstParsedTag)
rLocalDecl = ruleExpr
    [ tokA @"#let" <^> varA @"let_body"
        <:> \(tok1 :* body :* HNil) ->
            [||ParsedAst.DeclLet
                do unSpanned $$(body)
                do tokenSpan $$(tok1) <>> getSpan $$(body)
            ||]
    , tokA @"#rec" <^> varA @"let_body"
        <:> \(tok1 :* body :* HNil) ->
            [||ParsedAst.DeclRec
                do unSpanned $$(body)
                do tokenSpan $$(tok1) <>> getSpan $$(body)
            ||]
    ]

rLocalTypeDecl :: RuleExpr (ParsedAst.LocalDecl AstParsedTag)
rLocalTypeDecl = ruleExpr
    [ tokA @"#let" <^> varA @"let_type_body"
        <:> \(tok1 :* body :* HNil) ->
            [||ParsedAst.DeclLet
                do unSpanned $$(body)
                do tokenSpan $$(tok1) <>> getSpan $$(body)
            ||]
    ]

rLetBody :: RuleExpr (Spanned [ParsedAst.LetItem AstParsedTag])
rLetBody = ruleExpr
    [ varA @"lb_open" <^> varA @"let_body_items" <^> varA @"lb_close"
        <:> \(span1 :* items :* span2 :* HNil) ->
            [||Spanned
                {
                    unSpanned = unSpanned $$(items),
                    getSpan = $$(span1) <> getSpan $$(items) <> $$(span2)
                }
            ||]
    , varA @"let_body_item"
        <:> \(item :* HNil) ->
            [||Spanned
                {
                    unSpanned = [$$(item)],
                    getSpan = getSpan $$(item)
                }
            ||]
    ]

rLetBodyItems :: RuleExpr (Spanned [ParsedAst.LetItem AstParsedTag])
rLetBodyItems = undefined

rLetBodyItem :: RuleExpr (ParsedAst.LetItem AstParsedTag)
rLetBodyItem = undefined

rLetTypeBody :: RuleExpr (Spanned [ParsedAst.LetItem AstParsedTag])
rLetTypeBody = undefined

rLetTypeBodyItems :: RuleExpr (Spanned [ParsedAst.LetItem AstParsedTag])
rLetTypeBodyItems = undefined

rLetTypeBodyItem :: RuleExpr (ParsedAst.LetItem AstParsedTag)
rLetTypeBodyItem = undefined


rWhereBody :: RuleExpr (Spanned [ParsedAst.Decl AstParsedTag])
rWhereBody = undefined

rWhereBodyItems :: RuleExpr (Spanned [ParsedAst.Decl AstParsedTag])
rWhereBodyItems = undefined

rWhereBodyItem :: RuleExpr (ParsedAst.Decl AstParsedTag)
rWhereBodyItem = undefined


rBindExpr :: RuleExpr (ParsedAst.Decl AstParsedTag)
rBindExpr = undefined

rBindType :: RuleExpr (ParsedAst.Decl AstParsedTag)
rBindType = undefined

rBindPromType :: RuleExpr (ParsedAst.Decl AstParsedTag)
rBindPromType = undefined


rExpr :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExpr = undefined

rExprAnn :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprAnn = undefined

rExprInfix :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprInfix = undefined

rExprOp :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprOp = undefined

rExprApps :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprApps = undefined

rExprBlock :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprBlock = undefined

rExprAtomic :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprAtomic = undefined

rExprLiteral :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprLiteral = undefined


rCaseBody :: RuleExpr (Spanned [ParsedAst.CaseItem AstParsedTag])
rCaseBody = undefined

rCaseItems :: RuleExpr (Spanned [ParsedAst.CaseItem AstParsedTag])
rCaseItems = undefined

rCaseItem :: RuleExpr (ParsedAst.CaseItem AstParsedTag)
rCaseItem = undefined


rBlock :: RuleExpr (ParsedAst.Block AstParsedTag)
rBlock = undefined

rBlockItems :: RuleExpr (Spanned [ParsedAst.BlockItem AstParsedTag])
rBlockItems = undefined

rBlockItem :: RuleExpr (ParsedAst.BlockItem AstParsedTag)
rBlockItem = undefined

rBlockPats :: RuleExpr (Spanned [ParsedAst.Pat AstParsedTag])
rBlockPats = undefined

rBlockGuard :: RuleExpr (Spanned (ParsedAst.View AstParsedTag))
rBlockGuard = undefined

rBlockStmts :: RuleExpr (Spanned [ParsedAst.BlockStmt AstParsedTag])
rBlockStmts = undefined

rBlockStmt :: RuleExpr (ParsedAst.BlockStmt AstParsedTag)
rBlockStmt = undefined


rExprInterpString :: RuleExpr (ParsedAst.Expr AstParsedTag)
rExprInterpString = undefined


rExprTuple :: RuleExpr (ParsedAst.Tuple AstParsedTag)
rExprTuple = undefined

rExprTupleItems :: RuleExpr (Spanned [ParsedAst.TupleItem AstParsedTag])
rExprTupleItems = undefined

rExprTupleItem :: RuleExpr (ParsedAst.TupleItem AstParsedTag)
rExprTupleItem = undefined


rType :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rType = undefined

rTypeAnn :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeAnn = undefined

rTypeInfix :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeInfix = undefined

rTypeOp :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeOp = undefined

rTypeApps :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeApps = undefined

rTypeAtomic :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeAtomic = undefined

rTypeLiteral :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeLiteral = undefined


rBlockType :: RuleExpr (ParsedAst.Block AstParsedTag)
rBlockType = undefined

rBlockTypeStmts :: RuleExpr (Spanned [ParsedAst.BlockStmt AstParsedTag])
rBlockTypeStmts = undefined

rBlockTypeStmt :: RuleExpr (ParsedAst.BlockStmt AstParsedTag)
rBlockTypeStmt = undefined


rTypeTuple :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeTuple = undefined

rTypeTupleItems :: RuleExpr (Spanned [ParsedAst.TypeTupleItem AstParsedTag])
rTypeTupleItems = undefined

rTypeTupleItem :: RuleExpr (ParsedAst.TypeTupleItem AstParsedTag)
rTypeTupleItem = undefined


rTypeSigTuple :: RuleExpr (ParsedAst.TypeExpr AstParsedTag)
rTypeSigTuple = undefined

rTypeSigTupleItems :: RuleExpr (Spanned [ParsedAst.TypeTupleItem AstParsedTag])
rTypeSigTupleItems = undefined

rTypeSigTupleItem :: RuleExpr (ParsedAst.TypeTupleItem AstParsedTag)
rTypeSigTupleItem = undefined


rLbOpen :: RuleExpr Span
rLbOpen = ruleExpr
    [ varA @"lb_imp_open"
        <:> \(tok1 :* HNil) ->
            tok1
    , varA @"lb_exp_open"
        <:> \(tok1 :* HNil) ->
            tok1
    ]

rLbImpOpen :: RuleExpr Span
rLbImpOpen = ruleExpr
    [ tokA @"{"
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLbExpOpen :: RuleExpr Span
rLbExpOpen = ruleExpr
    [ tokA @"#{"
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLbClose :: RuleExpr Span
rLbClose = ruleExpr
    [ tokA @"}"
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLpOpen :: RuleExpr Span
rLpOpen = ruleExpr
    [ varA @"lp_imp_open"
        <:> \(tok1 :* HNil) ->
            tok1
    , varA @"lp_exp_open"
        <:> \(tok1 :* HNil) ->
            tok1
    ]

rLpImpOpen :: RuleExpr Span
rLpImpOpen = ruleExpr
    [ tokA @"("
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLpExpOpen :: RuleExpr Span
rLpExpOpen = ruleExpr
    [ tokA @"#("
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLpClose :: RuleExpr Span
rLpClose = ruleExpr
    [ tokA @")"
        <:> \(tok1 :* HNil) ->
            [||case tokenSpan $$(tok1) of
                Just x  -> x
                Nothing -> error "unreachable: expect a span."
            ||]
    ]

rLsemis :: RuleExpr (Maybe Span)
rLsemis = ruleExpr
    [ tokA @"<;>" <^> varA @"lsemis?"
        <:> \(tok1 :* span2 :* HNil) ->
            [||tokenSpan $$(tok1) <> $$(span2)||]
    , tokA @";" <^> varA @"lsemis?"
        <:> \(tok1 :* span2 :* HNil) ->
            [||tokenSpan $$(tok1) <> $$(span2)||]
    ]

rMayLsemis :: RuleExpr (Maybe Span)
rMayLsemis = ruleExpr
    [ tokA @"<;>" <^> varA @"lsemis?"
        <:> \(tok1 :* span2 :* HNil) ->
            [||tokenSpan $$(tok1) <> $$(span2)||]
    , tokA @";" <^> varA @"lsemis?"
        <:> \(tok1 :* span2 :* HNil) ->
            [||tokenSpan $$(tok1) <> $$(span2)||]
    , eps
        <:> \HNil ->
            [||Nothing||]
    ]
