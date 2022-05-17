{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Quell.Parsing.Parser.Rules where

import           Language.Quell.Prelude

import qualified Data.List                               as List
import qualified Language.Haskell.TH                     as TH
import qualified Language.Haskell.TH.Syntax                     as TH
import           Language.Parser.Ptera.TH                (eps, pattern (:*),
                                                          pattern HNil,
                                                          ruleExpr, varA,
                                                          (<::>), (<:>), (<^>))
import qualified Language.Parser.Ptera.TH                as Ptera
import qualified Language.Quell.Data.Bag                 as Bag
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Parsing.Spanned          as Spanned
import qualified Language.Quell.Type.Ast                 as Ast
import qualified Language.Quell.Type.Token               as Token
import           Language.Quell.Parsing.Parser.RulesLib


type Token = Layout.TokenWithL

pattern LexToken :: Token.LexToken -> Layout.TokenWithL
pattern LexToken t <- Layout.Token (Spanned.Spanned { unSpanned = t })

$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Token|]
    [ ("EOS",           [p|LexToken Token.EndOfSource|])

    , ("#as",           [p|LexToken Token.KwAs|])
    , ("#case",         [p|LexToken Token.KwCase|])
    , ("#data",         [p|LexToken Token.KwData|])
    , ("#default",      [p|LexToken Token.KwDefault|])
    , ("#derive",       [p|LexToken Token.KwDerive|])
    , ("#do",           [p|LexToken Token.KwDo|])
    , ("#export",       [p|LexToken Token.KwExport|])
    , ("#family",       [p|LexToken Token.KwFamily|])
    , ("#foreign",      [p|LexToken Token.KwForeign|])
    , ("#impl",         [p|LexToken Token.KwImpl|])
    , ("#in",           [p|LexToken Token.KwIn|])
    , ("#infix",        [p|LexToken Token.KwInfix|])
    , ("#let",          [p|LexToken Token.KwLet|])
    , ("#letrec",       [p|LexToken Token.KwLetrec|])
    , ("#match",        [p|LexToken Token.KwMatch|])
    , ("#module",       [p|LexToken Token.KwModule|])
    , ("#newtype",      [p|LexToken Token.KwNewtype|])
    , ("#pattern",      [p|LexToken Token.KwPattern|])
    , ("#rec",          [p|LexToken Token.KwRec|])
    , ("#record",       [p|LexToken Token.KwRecord|])
    , ("#role",         [p|LexToken Token.KwRole|])
    , ("#self",         [p|LexToken Token.KwSelf|])
    , ("#sig",          [p|LexToken Token.KwSignature|])
    , ("#static",       [p|LexToken Token.KwStatic|])
    , ("#trait",        [p|LexToken Token.KwTrait|])
    , ("#type",         [p|LexToken Token.KwType|])
    , ("#use",          [p|LexToken Token.KwUse|])
    , ("#with",         [p|LexToken Token.KwWith|])
    , ("#when",         [p|LexToken Token.KwWhen|])
    , ("#where",        [p|LexToken Token.KwWhere|])
    , ("#yield",        [p|LexToken Token.KwYield|])

    , ("#Default",      [p|LexToken Token.LKwDefault|])
    , ("#Self",         [p|LexToken Token.LKwSelf|])

    , ("@",             [p|LexToken Token.SymAt|])
    , ("!",             [p|LexToken Token.SymBang|])
    , (":",             [p|LexToken Token.SymColon|])
    , ("=",             [p|LexToken Token.SymEqual|])
    , ("^",             [p|LexToken Token.SymForall|])
    , ("\\",            [p|LexToken Token.SymLambda|])
    , ("|",             [p|LexToken Token.SymOr|])
    , ("~",             [p|LexToken Token.SymTilde|])
    , ("_",             [p|LexToken Token.SymUnderscore|])
    , ("?",             [p|LexToken Token.SymUnknown|])
    , ("#<",            [p|LexToken Token.SymBind|])
    , ("#>",            [p|LexToken Token.SymThen|])
    , ("##",            [p|LexToken Token.SymBlock|])
    , ("#@",            [p|LexToken Token.SymTypeBlock|])
    , ("#->",           [p|LexToken Token.SymArrow|])
    , ("#=>",           [p|LexToken Token.SymDerive|])

    , ("`",             [p|LexToken Token.SpBackquote|])
    , ("[",             [p|LexToken Token.SpBrackOpen|])
    , ("]",             [p|LexToken Token.SpBrackClose|])
    , (",",             [p|LexToken Token.SpComma|])
    , ("{",             [p|LexToken Token.SpBraceOpen|])
    , ("}",             [p|LexToken Token.SpBraceClose|])
    , ("{{",            [p|LexToken Token.SpDBraceOpen|])
    , ("}}",            [p|LexToken Token.SpDBraceClose|])
    , (".",             [p|LexToken Token.SpDot|])
    , ("..",            [p|LexToken Token.SpDots|])
    , ("(",             [p|LexToken Token.SpParenOpen|])
    , (")",             [p|LexToken Token.SpParenClose|])
    , (";",             [p|LexToken Token.SpSemi|])

    , ("con_id",        [p|LexToken Token.IdConId{}|])
    , ("con_sym",       [p|LexToken Token.IdConSym{}|])
    , ("var_id",        [p|LexToken Token.IdVarId{}|])
    , ("var_sym",       [p|LexToken Token.IdVarSym{}|])

    , ("bytechar",      [p|LexToken Token.LitByteChar{}|])
    , ("bytestring",    [p|LexToken Token.LitByteString{}|])
    , ("integer",       [p|LexToken Token.LitInteger{}|])
    , ("rational",      [p|LexToken Token.LitRational{}|])
    , ("char",          [p|LexToken Token.LitChar{}|])
    , ("string",        [p|LexToken Token.LitString{}|])

    , ("interp_string_without_interp",
                        [p|LexToken Token.InterpStringWithoutInterp{}|])
    , ("interp_string_start",
                        [p|LexToken Token.InterpStringStart{}|])
    , ("interp_string_cont",
                        [p|LexToken Token.InterpStringContinue{}|])
    , ("interp_string_end",
                        [p|LexToken Token.InterpStringEnd{}|])

    , ("{n}",           [p|Layout.ExpectNewImplicitLayout{}|])
    , ("<n>",           [p|Layout.Newline{}|])
    ])

$(Ptera.genRules
    do TH.mkName "RuleDefs"
    do Ptera.GenRulesTypes
        { Ptera.genRulesCtxTy = [t|GrammarContext|]
        , Ptera.genRulesTokensTy = [t|Tokens|]
        , Ptera.genRulesTokenTy = [t|Token|]
        }
    [ (TH.mkName "rdProgramEos", "program EOS", [t|Ast.Program AstParsed.T|])
    , (TH.mkName "rdProgram", "program", [t|Ast.Program AstParsed.T|])

    , (TH.mkName "rdDeclBody", "decl_body", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItems", "decl_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItemsWithSemis0", "(decl_item lsemis)* decl_item?",
        [t|(Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItem", "decl_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdTypeSigDecl", "typesig_decl", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdValSigDecl", "valsig_decl", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdConSigDecl", "consig_decl", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdTypeDecl", "type_decl", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdTypeDeclWhereBody", "type_decl_where_body", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeDeclWhereItems", "type_decl_where_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeDeclWhereItemsWithSemis0", "(type_decl_where_item lsemis)* type_decl_where_item?", [t|(Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeDeclWhereItem", "type_decl_where_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdDataDecl", "data_decl", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdDataDeclBody", "data_decl_body", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDataDeclItems", "data_decl_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDataDeclItemsWithSemis0", "(data_decl_item lsemis)* data_decl_item?", [t|(Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdDataDeclItem", "data_decl_item", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdAlgDataType", "alg_data_type", [t|([Ast.ConType AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdAlgDataTypeItems", "alg_data_type_items", [t|([Ast.ConType AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdConTypesWithBars0", "(contype '|')* contype?", [t|(Bag.T (Ast.ConType AstParsed.T), Maybe Spanned.Span)|])

    , (TH.mkName "rdValDecl", "val_decl", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdValBind", "val_bind", [t|Ast.Decl AstParsed.T|])
    , (TH.mkName "rdValDeclWhereBody", "val_decl_where_body", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdValDeclWhereItems", "val_decl_where_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdValDeclWhereItemsWithSemis0", "(val_decl_where_item lsemis)* val_decl_where_item?", [t|(Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdValDeclWhereItem", "val_decl_where_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdDeclType", "decltype", [t|Ast.DeclType AstParsed.T|])
    , (TH.mkName "rdConType", "contype", [t|Ast.ConType AstParsed.T|])
    , (TH.mkName "rdDeclVarExpr", "declvarexpr", [t|Ast.DeclExpr AstParsed.T|])

    , (TH.mkName "rdType", "type", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeInfix", "type_infix", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeOp", "type_op", [t|(Ast.TypeExpr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdTypeOpBlock", "type_op_block", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeOpSymQualified", "type_op_sym_qualified", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeApps", "type_apps", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeApps0", "type_app*", [t|(Bag.T (Ast.AppType AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeApps1", "type_app+", [t|(Bag.T (Ast.AppType AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdTypeApp", "type_app", [t|Ast.AppType AstParsed.T|])
    , (TH.mkName "rdTypeQualified", "type_qualified", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeQualifieds0", "type_qualified*", [t|(Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeBlock", "type_block", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeAtomic", "type_atomic", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeLiteral", "type_literal", [t|Ast.TypeExpr AstParsed.T|])
    , (TH.mkName "rdTypeBlockBody", "type_block_body", [t|(Ast.TypeExpr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdTypeBlockItem", "type_block_item", [t|(Ast.TypeExpr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdTypeTupleItems", "type_tuple_items", [t|([Ast.TypeExpr AstParsed.T], Spanned.Span)|])
    , (TH.mkName "rdTypeArrayItems", "type_array_items", [t|([Ast.TypeExpr AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeSimpleRecordItems", "type_simplrecord_items", [t|([Ast.TypeRecordItem AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeSimpleRecordItemsWithCommas0", "(type_simplrecord_item ',')* type_simplrecord_item?", [t|(Bag.T (Ast.TypeRecordItem AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdTypeSimpleRecordItem", "type_simplrecord_item", [t|Ast.TypeRecordItem AstParsed.T|])
    , (TH.mkName "rdTypesWithCommas2", "(type ',')+ type ','?", [t|(Bag.T (Ast.TypeExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdTypesWithCommas0", "(type ',')* type?", [t|(Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)|])

    , (TH.mkName "rdSigItem", "sig_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdExpr", "expr", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprInfix", "expr_infix", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprOp", "expr_op", [t|(Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdExprOpBlock", "expr_op_block", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprOpSymQualified", "expr_op_sym_qualified", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprApps", "expr_apps", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprApps1", "expr_app+", [t|(Bag.T (Ast.AppExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdExprApp", "expr_app", [t|Ast.AppExpr AstParsed.T|])
    , (TH.mkName "rdExprQualified", "expr_qualified", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprBlock", "expr_block", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprAtomic", "expr_atomic", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprLiteral", "expr_literal", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprBlockBody", "expr_block_body", [t|(Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdExprBlockItem", "expr_block_item", [t|(Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdExprInterpString", "expr_interp_string", [t|Ast.Expr AstParsed.T|])
    , (TH.mkName "rdExprInterpStringContParts", "(interp_string_cont expr)* interp_string_end", [t|(Bag.T (Ast.InterpStringPart AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdExprMatchItems", "expr_match_items", [t|([Ast.Expr AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdExprTupleItems", "expr_tuple_items", [t|([Ast.Expr AstParsed.T], Spanned.Span)|])
    , (TH.mkName "rdExprArrayItems", "expr_array_items", [t|([Ast.Expr AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdExprsWithCommas0", "(expr ',')* expr?", [t|(Bag.T (Ast.Expr AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdExprsWithCommas2", "(expr ',')+ expr ','?", [t|(Bag.T (Ast.Expr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdExprSimpleRecordItems", "expr_simplrecord_items", [t|([Ast.ExprRecordItem AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdExprSimpleRecordItemsWithCommas0", "(expr_simplrecord_item ',')* expr_simplrecord_item?", [t|(Bag.T (Ast.ExprRecordItem AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdExprSimpleRecordItem", "expr_simplrecord_item", [t|Ast.ExprRecordItem AstParsed.T|])

    , (TH.mkName "rdPat", "pat", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatsWithCommas0", "(pat ',')* pat?", [t|(Bag.T (Ast.Pat AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdPatsWithCommas2", "(pat ',')+ pat ','?", [t|(Bag.T (Ast.Pat AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdPatUnit", "pat_unit", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatInfixesWithBars1", "(pat_infix '|')* pat_infix '|'?", [t|(Bag.T (Ast.Pat AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdPatInfix", "pat_infix", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatOp", "pat_op", [t|(Ast.PatOp AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdPatOpBlock", "pat_op_block", [t|Ast.PatOp AstParsed.T|])
    , (TH.mkName "rdPatOpSymQualified", "pat_op_sym_qualified", [t|Ast.PatOp AstParsed.T|])
    , (TH.mkName "rdPatApps", "pat_apps", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatApp", "pat_app", [t|Ast.AppPat AstParsed.T|])
    , (TH.mkName "rdPatApps0", "pat_app*", [t|(Bag.T (Ast.AppPat AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdPatUnivApp", "pat_univ_app", [t|(Ast.TypeExpr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdPatUnivApps0", "pat_univ_app*", [t|(Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdPatQualified", "pat_qualified", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatBlock", "pat_block", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatAtomic", "pat_atomic", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatAtomics0", "pat_atomic*", [t|(Bag.T (Ast.Pat AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdPatLiteral", "pat_literal", [t|Ast.Pat AstParsed.T|])
    , (TH.mkName "rdPatBlockBody", "pat_block_body", [t|(Ast.Pat AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdPatBlockItems", "pat_block_items", [t|(Ast.Pat AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdPatTupleItems", "pat_tuple_items", [t|([Ast.Pat AstParsed.T], Spanned.Span)|])
    , (TH.mkName "rdPatArrayItems", "pat_array_items", [t|([Ast.Pat AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdPatSimpleRecordItems", "pat_simplrecord_items", [t|([Ast.PatRecordItem AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdPatSimpleRecordItem", "pat_simplrecord_item", [t|Ast.PatRecordItem AstParsed.T|])
    , (TH.mkName "rdPatSimpleRecordItemsWithCommas0", "(pat_simplrecord_item ',')* pat_simplrecord_item?", [t|(Bag.T (Ast.PatRecordItem AstParsed.T), Maybe Spanned.Span)|])

    , (TH.mkName "rdLetBinds", "let_binds", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdLetBindItems", "let_bind_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdLetBindItemsWithSemis0", "(let_bind_item lsemis)* let_bind_item?", [t|(Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdLetBindItem", "let_bind_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdCaseAltBody", "case_alt_body", [t|([Ast.CaseAlt AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdCaseAltItems", "case_alt_items", [t|([Ast.CaseAlt AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdCaseAltItemsWithSemis0", "(case_alt_item lsemis)* case_alt_item?", [t|(Bag.T (Ast.CaseAlt AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdCaseAltItem", "case_alt_item", [t|Ast.CaseAlt AstParsed.T|])
    , (TH.mkName "rdGuardedAlts", "guarded_alts", [t|([Ast.GuardedAlt AstParsed.T], Spanned.Span)|])
    , (TH.mkName "rdGuardedAltBody", "guarded_alt_body", [t|([Ast.GuardedAlt AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdGuardedAltItems", "guarded_alt_items", [t|([Ast.GuardedAlt AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdGuardedAltItemsWithSemis0", "(guarded_alt_item lsemis)* guarded_alt_item?", [t|(Bag.T (Ast.GuardedAlt AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdGuardedAltItem", "guarded_alt_item", [t|Ast.GuardedAlt AstParsed.T|])
    , (TH.mkName "rdGuardQual", "guard_qual", [t|Ast.Expr AstParsed.T|])

    , (TH.mkName "rdDoBody", "do_body", [t|([Ast.DoStmt AstParsed.T], Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdDoStmtItems", "do_stmt_items", [t|([Ast.DoStmt AstParsed.T], Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdDoStmtItemsWithSemis1", "(do_stmt_item lsemis)* do_yield_item lsemis?", [t|(Bag.T (Ast.DoStmt AstParsed.T), Ast.Expr AstParsed.T, Spanned.Span)|])
    , (TH.mkName "rdDoStmtItem", "do_stmt_item", [t|Ast.DoStmt AstParsed.T|])
    , (TH.mkName "rdDoYieldItem", "do_yield_item", [t|(Ast.Expr AstParsed.T, Spanned.Span)|])

    , (TH.mkName "rdBindVar", "bind_var", [t|Ast.BindVar AstParsed.T|])
    , (TH.mkName "rdBindVars0", "bind_var*", [t|(Bag.T (Ast.BindVar AstParsed.T), Maybe Spanned.Span)|])
    , (TH.mkName "rdActualBindVar", "actual_bind_var", [t|Ast.BindVar AstParsed.T|])
    , (TH.mkName "rdSimpleBindVar", "simple_bind_var", [t|(Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdBlockBindVar", "block_bind_var", [t|(Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdBlockBindVarItems", "block_bind_var_items", [t|(Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdBlockBindVarItem", "block_bind_var_item", [t|(Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)|])
    , (TH.mkName "rdConQualified", "con_qualified", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdConOpQualified", "conop_qualified", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdCon", "con", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdConOp", "conop", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdVar", "var", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdConIdExt", "con_id_ext", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdConSymExt", "con_sym_ext", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdVarIdExt", "var_id_ext", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdVarSymExt", "var_sym_ext", [t|(Ast.Name, Spanned.Span)|])

    , (TH.mkName "rdDeclCon", "declcon", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdDeclConOp", "declconop", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdDeclVar", "declvar", [t|(Ast.Name, Spanned.Span)|])
    , (TH.mkName "rdDeclOp", "declop", [t|(Ast.Name, Spanned.Span)|])

    , (TH.mkName "rdLsemis", "lsemis", [t|Maybe Spanned.Span|])
    , (TH.mkName "rdMayLsemis", "lsemis?", [t|Maybe Spanned.Span|])
    , (TH.mkName "rdLsemi", "lsemi", [t|Maybe Spanned.Span|])

    , (TH.mkName "rdImpBc", "imp_bc", [t|()|])
    , (TH.mkName "rdSkip", "skip", [t|()|])

    , (TH.mkName "rdLiteral", "literal", [t|Ast.Lit AstParsed.T|])
    , (TH.mkName "rdMayTypeSig", "(':' type)?", [t|(Maybe (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)|])

    , (TH.mkName "rdTokOpenParen", "tok_open_paren_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokOpenBrack", "tok_open_brack_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokOpenBrace", "tok_open_brace_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokOpenDBrace", "tok_open_dbrace_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokCloseParen", "tok_close_paren_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokCloseBrack", "tok_close_brack_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokCloseBrace", "tok_close_brace_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokCloseDBrace", "tok_close_dbrace_without_skip", [t|Spanned.Span|])
    , (TH.mkName "rdTokInterpStringStart", "tok_interp_string_start_without_skip", [t|Ast.InterpStringPart AstParsed.T|])
    , (TH.mkName "rdTokInterpStringCont", "tok_interp_string_cont_without_skip", [t|Ast.InterpStringPart AstParsed.T|])
    , (TH.mkName "rdTokInterpStringEnd", "tok_interp_string_end_without_skip", [t|Ast.InterpStringPart AstParsed.T|])
    , (TH.mkName "rdTokNewImplicitLayout", "tok_new_implicit_layout_without_skip", [t|()|])
    ])

$(Ptera.genParsePoints
    do TH.mkName "ParsePoints"
    do TH.mkName "RuleDefs"
    [ "program EOS"
    , "type"
    , "expr"
    ])

type RuleExpr = Ptera.RuleExprM GrammarContext RuleDefs Tokens Token
type Alt = Ptera.AltM GrammarContext RuleDefs Tokens Token
type TypedExpr = Ptera.TypedExpr RuleDefs Tokens Token
type SemAct = Ptera.SemActM GrammarContext
type ActionTask = Ptera.ActionTask GrammarContext


grammar :: Ptera.GrammarM GrammarContext RuleDefs Tokens Token ParsePoints
grammar = Ptera.fixGrammar
    do RuleDefs
        { rdTokNewImplicitLayout = ruleSymbolRule do Proxy @"{n}"
        , rdTokInterpStringStart = ruleSymbolRule do Proxy @"interp_string_start"
        , rdTokInterpStringCont = ruleSymbolRule do Proxy @"interp_string_cont"
        , rdTokInterpStringEnd = ruleSymbolRule do Proxy @"interp_string_end"
        , rdTokOpenDBrace = ruleSymbolRule do Proxy @"{{"
        , rdTokOpenBrace = ruleSymbolRule do Proxy @"{"
        , rdTokOpenBrack = ruleSymbolRule do Proxy @"["
        , rdTokOpenParen = ruleSymbolRule do Proxy @"("
        , rdTokCloseDBrace = ruleSymbolRule do Proxy @"}}"
        , rdTokCloseBrace = ruleSymbolRule do Proxy @"}"
        , rdTokCloseBrack = ruleSymbolRule do Proxy @"]"
        , rdTokCloseParen = ruleSymbolRule do Proxy @")"
        , rdMayTypeSig = rMayTypeSig
        , rdLiteral = rLiteral
        , rdSkip = rSkip
        , rdImpBc = rImpBc
        , rdLsemi = rLsemi
        , rdMayLsemis = rMayLsemis
        , rdLsemis = rLsemis
        , rdDeclOp = rDeclOp
        , rdDeclVar = rDeclVar
        , rdDeclConOp = rDeclConOp
        , rdDeclCon = rDeclCon
        , rdVarSymExt = rVarSymExt
        , rdConSymExt = rConSymExt
        , rdVar = rVar
        , rdCon = rCon
        , rdConOpQualified = rConOpQualified
        , rdConQualified = rConQualified
        , rdActualBindVar = rActualBindVar
        , rdBindVars0 = rBindVars0
        , rdBindVar = rBindVar
        , rdDoBody = rDoBody
        , rdGuardedAlts = rGuardedAlts
        , rdCaseAltBody = rCaseAltBody
        , rdLetBindItem = rLetBindItem
        , rdLetBinds = rLetBinds
        , rdPatAtomics0 = rPatAtomics0
        , rdPatAtomic = rPatAtomic
        , rdPatApps0 = rPatApps0
        , rdPatApps = rPatApps
        , rdPatOpSymQualified = rPatOpSymQualified
        , rdPatOpBlock = rPatOpBlock
        , rdPatOp = rPatOp
        , rdPatInfix = rPatInfix
        , rdPatInfixesWithBars1 = rPatInfixesWithBars1
        , rdPatUnit = rPatUnit
        , rdPat = rPat
        , rdExprSimpleRecordItem = rExprSimpleRecordItem
        , rdExprSimpleRecordItemsWithCommas0 = rExprSimpleRecordItemsWithCommas0
        , rdExprSimpleRecordItems = rExprSimpleRecordItems
        , rdExprsWithCommas2 = rExprsWithCommas2
        , rdExprsWithCommas0 = rExprsWithCommas0
        , rdExprArrayItems = rExprArrayItems
        , rdExprTupleItems = rExprTupleItems
        , rdExprMatchItems = rExprMatchItems
        , rdExprInterpStringContParts = rExprInterpStringContParts
        , rdExprInterpString = rExprInterpString
        , rdExprBlockItem = rExprBlockItem
        , rdExprBlockBody = rExprBlockBody
        , rdExprLiteral = rExprLiteral
        , rdExprAtomic = rExprAtomic
        , rdExprBlock = rExprBlock
        , rdExprQualified = rExprQualified
        , rdExprApp = rExprApp
        , rdExprApps1 = rExprApps1
        , rdExprApps = rExprApps
        , rdExprOpSymQualified = rExprOpSymQualified
        , rdExprOpBlock = rExprOpBlock
        , rdExprOp = rExprOp
        , rdExprInfix = rExprInfix
        , rdExpr = rExpr
        , rdSigItem = rSigItem
        , rdTypesWithCommas0 = rTypesWithCommas0
        , rdTypesWithCommas2 = rTypesWithCommas2
        , rdTypeSimpleRecordItem = rTypeSimpleRecordItem
        , rdTypeSimpleRecordItemsWithCommas0 = rTypeSimpleRecordItemsWithCommas0
        , rdTypeSimpleRecordItems = rTypeSimpleRecordItems
        , rdTypeArrayItems = rTypeArrayItems
        , rdTypeTupleItems = rTypeTupleItems
        , rdTypeBlockItem = rTypeBlockItem
        , rdTypeBlockBody = rTypeBlockBody
        , rdTypeLiteral = rTypeLiteral
        , rdTypeAtomic = rTypeAtomic
        , rdTypeBlock = rTypeBlock
        , rdTypeQualifieds0 = rTypeQualifieds0
        , rdTypeQualified = rTypeQualified
        , rdTypeApp = rTypeApp
        , rdTypeApps1 = rTypeApps1
        , rdTypeApps0 = rTypeApps0
        , rdTypeApps = rTypeApps
        , rdTypeOpSymQualified = rTypeOpSymQualified
        , rdTypeOpBlock = rTypeOpBlock
        , rdTypeOp = rTypeOp
        , rdTypeInfix = rTypeInfix
        , rdType = rType
        , rdDeclVarExpr = rDeclVarExpr
        , rdConType = rConType
        , rdDeclType = rDeclType
        , rdValDeclWhereItem = rValDeclWhereItem
        , rdValDeclWhereItemsWithSemis0 = rValDeclWhereItemsWithSemis0
        , rdValDeclWhereBody = rValDeclWhereBody
        , rdValDecl = rValDecl
        , rdConTypesWithBars0 = rConTypesWithBars0
        , rdAlgDataTypeItems = rAlgDataTypeItems
        , rdAlgDataType = rAlgDataType
        , rdDataDeclItem = rDataDeclItem
        , rdDataDeclItemsWithSemis0 = rDataDeclItemsWithSemis0
        , rdDataDeclItems = rDataDeclItems
        , rdValDeclWhereItems = rValDeclWhereItems
        , rdDataDeclBody = rDataDeclBody
        , rdDataDecl = rDataDecl
        , rdTypeDeclWhereItem = rTypeDeclWhereItem
        , rdTypeDeclWhereItemsWithSemis0 = rTypeDeclWhereItemsWithSemis0
        , rdTypeDeclWhereItems = rTypeDeclWhereItems
        , rdTypeDeclWhereBody = rTypeDeclWhereBody
        , rdTypeDecl = rTypeDecl
        , rdConSigDecl = rConSigDecl
        , rdValSigDecl = rValSigDecl
        , rdTypeSigDecl = rTypeSigDecl
        , rdDeclItem = rDeclItem
        , rdDeclItemsWithSemis0 = rDeclItemsWithSemis0
        , rdDeclItems = rDeclItems
        , rdDeclBody = rDeclBody
        , rdProgram = rProgram
        , rdProgramEos = rProgramEos
        , rdDoYieldItem = rDoYieldItem
        , rdDoStmtItem = rDoStmtItem
        , rdDoStmtItemsWithSemis1 = rDoStmtItemsWithSemis1
        , rdDoStmtItems = rDoStmtItems
        , rdLetBindItemsWithSemis0 = rLetBindItemsWithSemis0
        , rdLetBindItems = rLetBindItems
        , rdValBind = rValBind
        , rdGuardQual = rGuardQual
        , rdGuardedAltItem = rGuardedAltItem
        , rdGuardedAltItemsWithSemis0 = rGuardedAltItemsWithSemis0
        , rdGuardedAltItems = rGuardedAltItems
        , rdGuardedAltBody = rGuardedAltBody
        , rdCaseAltItem = rCaseAltItem
        , rdCaseAltItemsWithSemis0 = rCaseAltItemsWithSemis0
        , rdCaseAltItems = rCaseAltItems
        , rdPatsWithCommas0 = rPatsWithCommas0
        , rdSimpleBindVar = rSimpleBindVar
        , rdBlockBindVar = rBlockBindVar
        , rdBlockBindVarItems = rBlockBindVarItems
        , rdBlockBindVarItem = rBlockBindVarItem
        , rdVarIdExt = rVarIdExt
        , rdConIdExt = rConIdExt
        , rdConOp = rConOp
        , rdPatQualified = rPatQualified
        , rdPatUnivApps0 = rPatUnivApps0
        , rdPatUnivApp = rPatUnivApp
        , rdPatApp = rPatApp
        , rdPatBlock = rPatBlock
        , rdPatBlockBody = rPatBlockBody
        , rdPatLiteral = rPatLiteral
        , rdPatBlockItems = rPatBlockItems
        , rdPatSimpleRecordItems = rPatSimpleRecordItems
        , rdPatArrayItems = rPatArrayItems
        , rdPatTupleItems = rPatTupleItems
        , rdPatSimpleRecordItemsWithCommas0 = rPatSimpleRecordItemsWithCommas0
        , rdPatSimpleRecordItem = rPatSimpleRecordItem
        , rdPatsWithCommas2 = rPatsWithCommas2
        }


rProgramEos :: RuleExpr (Ast.Program AstParsed.T)
rProgramEos = ruleExpr
    [ varA @"program" <^> tokA @"EOS"
        <:> \(program :* _ :* _ :* HNil) ->
            program
    ]

rProgram :: RuleExpr (Ast.Program AstParsed.T)
rProgram = ruleExpr
    [ varA @"decl_body"
        <:> \(body :* HNil) ->
            [||case $$(body) of { (items, msItems) ->
                Ast.Program items msItems
            }||]
    ]


rDeclBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDeclBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"decl_items" <^> tokVarA @"}}"
        <:> \(_ :* kodbrace :* itemsE :* _ :* kcdbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do
                    AstParsed.sp ($$(kodbrace) AstParsed.:<< msItems, $$(kcdbrace))
                )
            }||]
    , tokVarA @"{" <^> varA @"decl_items" <^> tokVarA @"}"
        <:> \(_ :* kodbrace :* itemsE :* _ :* kcdbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kodbrace) AstParsed.:<< msItems, $$(kcdbrace))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"decl_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rDeclItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDeclItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(decl_item lsemis)* decl_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp [$$(mayLsemis), msItems]
                )
            }||]
    ]

rDeclItemsWithSemis0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rDeclItemsWithSemis0 = ruleExpr
    [ varA @"decl_item" <^> varA @"lsemis" <^> varA @"(decl_item lsemis)* decl_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"decl_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rDeclItem :: RuleExpr (Ast.Decl AstParsed.T)
rDeclItem = ruleExpr
    [ varA @"type_decl"
        <:> \(typeDecl :* HNil) ->
            typeDecl
    , varA @"data_decl"
        <:> \(dataDecl :* HNil) ->
            dataDecl
    , varA @"val_decl"
        <:> \(valDecl :* HNil) ->
            valDecl
    , varA @"sig_item"
        <:> \(sigItem :* HNil) ->
            sigItem
    ]


rTypeSigDecl :: RuleExpr (Ast.Decl AstParsed.T)
rTypeSigDecl = ruleExpr
    [ tokA @"#type" <^> varA @"declcon" <^> tokA @":" <^> varA @"type"
        <:> \(_ :* ktype :* declconE :* _ :* kcolon :* ty :* HNil) ->
            [||case $$(declconE) of { (declcon, spDeclCon) ->
                Ast.DeclTypeSig declcon $$(ty)
                    do AstParsed.sp
                        ( lexToken $$(ktype), spDeclCon
                        , lexToken $$(kcolon), $$(ty)
                        )
            }||]
    ]

rValSigDecl :: RuleExpr (Ast.Decl AstParsed.T)
rValSigDecl = ruleExpr
    [ varA @"declvar" <^> tokA @":" <^> varA @"type"
        <:> \(declvarE :* _ :* kcolon :* ty :* HNil) ->
            [||case $$(declvarE) of { (declvar, spDeclVar) ->
                Ast.DeclValSig declvar $$(ty)
                    do AstParsed.sp (spDeclVar, lexToken $$(kcolon), $$(ty))
            }||]
    ]

rConSigDecl :: RuleExpr (Ast.Decl AstParsed.T)
rConSigDecl = ruleExpr
    [ varA @"declcon" <^> tokA @":" <^> varA @"type"
        <:> \(declconE :* _ :* kcolon :* ty :* HNil) ->
            [||case $$(declconE) of { (declcon, spDeclCon) ->
                Ast.DeclConSig declcon $$(ty) do
                    AstParsed.sp (spDeclCon, lexToken $$(kcolon), $$(ty))
            }||]
    ]


rTypeDecl :: RuleExpr (Ast.Decl AstParsed.T)
rTypeDecl = ruleExpr
    [ tokA @"#type" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(_ :* ktype :* decltype :* _ :* keq :* ty :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclType $$(decltype) $$(ty) whereItems
                    do AstParsed.sp
                        ( lexToken $$(ktype), $$(decltype)
                        , lexToken $$(keq), $$(ty)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , tokA @"#type" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type"
        <:> \(_ :* ktype :* decltype :* _ :* keq :* ty :* HNil) ->
            [||
                Ast.DeclType $$(decltype) $$(ty) []
                    do AstParsed.sp
                        ( lexToken $$(ktype), $$(decltype)
                        , lexToken $$(keq), $$(ty)
                        )
            ||]
    ]

rTypeDeclWhereBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rTypeDeclWhereBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"type_decl_where_items" <^> tokVarA @"}}"
        <:> \(_ :* kodbrace :* itemsE :* _ :* kcdbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kodbrace), msItems AstParsed.:>> $$(kcdbrace))
                )
            }||]
    , tokVarA @"{" <^> varA @"type_decl_where_items" <^> tokVarA @"}"
        <:> \(_ :* kobrace :* itemsE :* _ :* kcbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kobrace), msItems AstParsed.:>> $$(kcbrace))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"type_decl_where_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rTypeDeclWhereItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rTypeDeclWhereItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(type_decl_where_item lsemis)* type_decl_where_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rTypeDeclWhereItemsWithSemis0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rTypeDeclWhereItemsWithSemis0 = ruleExpr
    [ varA @"type_decl_where_item" <^> varA @"lsemis" <^> varA @"(type_decl_where_item lsemis)* type_decl_where_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"type_decl_where_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rTypeDeclWhereItem :: RuleExpr (Ast.Decl AstParsed.T)
rTypeDeclWhereItem = ruleExpr
    [ varA @"type_decl"
        <:> \(typeDecl :* HNil) ->
            typeDecl
    , varA @"typesig_decl"
        <:> \(typeSigDecl :* HNil) ->
            typeSigDecl
    ]


rDataDecl :: RuleExpr (Ast.Decl AstParsed.T)
rDataDecl = ruleExpr
    [ tokA @"#data" <^> varA @"decltype" <^> tokA @"=" <^> varA @"alg_data_type" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(_ :* kdata :* decltype :* _ :* keq :* itemsE :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                case $$(whereBody) of { (whereItems, msWhereItems) ->
                    Ast.DeclAlgDataType $$(decltype) items whereItems
                        do AstParsed.sp
                            ( lexToken $$(kdata), $$(decltype)
                            , lexToken $$(keq) AstParsed.:<< msItems
                            , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                            )
                }
            }||]
    , tokA @"#data" <^> varA @"decltype" <^> tokA @"=" <^> varA @"alg_data_type"
        <:> \(_ :* kdata :* decltype :* _ :* keq :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.DeclAlgDataType $$(decltype) items []
                    do AstParsed.sp
                        ( lexToken $$(kdata), $$(decltype)
                        , lexToken $$(keq) AstParsed.:<< msItems
                        )
            }||]
    , tokA @"#data" <^> varA @"decltype" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(_ :* kdata :* decltype :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclAlgDataType $$(decltype) [] whereItems
                    do AstParsed.sp
                        ( lexToken $$(kdata), $$(decltype)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , tokA @"#data" <^> varA @"decltype"
        <:> \(_ :* kdata :* decltype :* HNil) ->
            [||
                Ast.DeclAlgDataType $$(decltype) [] []
                    do AstParsed.sp
                        ( lexToken $$(kdata), $$(decltype)
                        )
            ||]
    , tokA @"#data" <^> varA @"declcon" <^> varA @"(':' type)?" <^> tokA @"#where" <^> varA @"data_decl_body"
        <:> \(_ :* kdata :* declconE :* mayTyE :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(declconE) of { (declcon, spDeclcon) ->
                case $$(mayTyE) of { (mayTy, msMayTy) ->
                    case $$(whereBody) of { (whereItems, msWhereItems) ->
                        Ast.DeclDataType declcon mayTy whereItems
                            do AstParsed.sp
                                ( lexToken $$(kdata), spDeclcon AstParsed.:<< msMayTy
                                , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                                )
                    }
                }
            }||]
    , tokA @"#data" <^> varA @"declcon" <^> varA @"(':' type)?"
        <:> \(_ :* kdata :* declconE :* mayTyE :* HNil) ->
            [||case $$(declconE) of { (declcon, spDeclcon) ->
                case $$(mayTyE) of { (mayTy, msMayTy) ->
                    Ast.DeclDataType declcon mayTy []
                        do AstParsed.sp
                            ( lexToken $$(kdata), spDeclcon AstParsed.:<< msMayTy
                            )
                }
            }||]
    , tokA @"#newtype" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(_ :* knewtype :* decltype :* _ :* keq :* ty :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclNewType $$(decltype) $$(ty) whereItems
                    do AstParsed.sp
                        ( lexToken $$(knewtype), $$(decltype)
                        , lexToken $$(keq), $$(ty)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , tokA @"#newtype" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type"
        <:> \(_ :* knewtype :* decltype :* _ :* keq :* ty :* HNil) ->
            [||
                Ast.DeclNewType $$(decltype) $$(ty) []
                    do AstParsed.sp
                        ( lexToken $$(knewtype), $$(decltype)
                        , lexToken $$(keq), $$(ty)
                        )
            ||]
    ]

rDataDeclBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDataDeclBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"data_decl_items" <^> tokVarA @"}}"
        <:> \(_ :* kodbrace :* itemsE :* _ :* kcdbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kodbrace), msItems AstParsed.:>> $$(kcdbrace))
                )
            }||]
    , tokVarA @"{" <^> varA @"data_decl_items" <^> tokVarA @"}"
        <:> \(_ :* kobrace :* itemsE :* _ :* kcbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kobrace), msItems AstParsed.:>> $$(kcbrace))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"data_decl_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rDataDeclItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDataDeclItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(data_decl_item lsemis)* data_decl_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rDataDeclItemsWithSemis0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rDataDeclItemsWithSemis0 = ruleExpr
    [ varA @"data_decl_item" <^> varA @"lsemis" <^> varA @"(data_decl_item lsemis)* data_decl_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"data_decl_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rDataDeclItem :: RuleExpr (Ast.Decl AstParsed.T)
rDataDeclItem = ruleExpr
    [ varA @"type_decl"
        <:> \(typeDecl :* HNil) ->
            typeDecl
    , varA @"typesig_decl"
        <:> \(typeSigDecl :* HNil) ->
            typeSigDecl
    , varA @"consig_decl"
        <:> \(conSigDecl :* HNil) ->
            conSigDecl
    ]

rAlgDataType :: RuleExpr ([Ast.ConType AstParsed.T], Maybe Spanned.Span)
rAlgDataType = ruleExpr
    [ tokVarA @"(" <^> varA @"alg_data_type_items" <^> tokVarA @")"
        <:> \(_ :* kop :* itemsE :* _ :* kcp :* HNil) ->
            [||case $$(itemsE) of { (items, ms) ->
                ( items
                , Just do AstParsed.sp ($$(kop) AstParsed.:<< ms, $$(kcp))
                )
            }||]
    , varA @"alg_data_type_items"
        <:> \(items :* HNil) ->
            items
    ]

rAlgDataTypeItems :: RuleExpr ([Ast.ConType AstParsed.T], Maybe Spanned.Span)
rAlgDataTypeItems = ruleExpr
    [ tokA @"|" <^> varA @"(contype '|')* contype?"
        <:> \(_ :* kmid :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kmid) AstParsed.:<< msItems
                )
            }||]
    , varA @"(contype '|')* contype?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rConTypesWithBars0 :: RuleExpr (Bag.T (Ast.ConType AstParsed.T), Maybe Spanned.Span)
rConTypesWithBars0 = ruleExpr
    [ varA @"contype" <^> tokA @"|" <^> varA @"(contype '|')* contype?"
        <:> \(item :* _ :* kmid :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp ($$(item), lexToken $$(kmid) AstParsed.:<< msItems)
                )
            }||]
    , varA @"contype"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]


rValDecl :: RuleExpr (Ast.Decl AstParsed.T)
rValDecl = ruleExpr
    [ varA @"declvarexpr" <^> tokA @"=" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(declVarExpr :* _ :* keq :* expr :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclVal $$(declVarExpr) $$(expr) whereItems
                    do AstParsed.sp
                        ( $$(declVarExpr), lexToken $$(keq), $$(expr)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , varA @"declvarexpr" <^> tokA @"=" <^> varA @"expr"
        <:> \(declVarExpr :* _ :* keq :* expr :* HNil) ->
            [||
                Ast.DeclVal $$(declVarExpr) $$(expr) []
                    do AstParsed.sp ($$(declVarExpr), lexToken $$(keq), $$(expr))
            ||]
    ]

rValBind :: RuleExpr (Ast.Decl AstParsed.T)
rValBind = ruleExpr
    [ varA @"pat" <^> tokA @"=" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(pat :* _ :* keq :* expr :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclValBind $$(pat) $$(expr) whereItems
                    do AstParsed.sp
                        ( $$(pat), lexToken $$(keq), $$(expr)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , varA @"pat" <^> tokA @"=" <^> varA @"expr"
        <:> \(pat :* _ :* keq :* expr :* HNil) ->
            [||
                Ast.DeclValBind $$(pat) $$(expr) []
                    do AstParsed.sp ($$(pat), lexToken $$(keq), $$(expr))
            ||]
    ]

rValDeclWhereBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rValDeclWhereBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"val_decl_where_items" <^> tokVarA @"}}"
        <:> \(_ :* kodbrace :* itemsE :* _ :* kcdbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kodbrace), msItems AstParsed.:>> $$(kcdbrace))
                )
            }||]
    , tokVarA @"{" <^> varA @"val_decl_where_items" <^> tokVarA @"}"
        <:> \(_ :* kobrace :* itemsE :* _ :* kcbrace :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kobrace), msItems AstParsed.:>> $$(kcbrace))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"val_decl_where_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rValDeclWhereItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rValDeclWhereItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(val_decl_where_item lsemis)* val_decl_where_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rValDeclWhereItemsWithSemis0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rValDeclWhereItemsWithSemis0 = ruleExpr
    [ varA @"val_decl_where_item" <^> varA @"lsemis" <^> varA @"(val_decl_where_item lsemis)* val_decl_where_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"val_decl_where_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rValDeclWhereItem :: RuleExpr (Ast.Decl AstParsed.T)
rValDeclWhereItem = ruleExpr
    [ varA @"let_bind_item"
        <:> \(declItem :* HNil) ->
            declItem
    ]


rDeclType :: RuleExpr (Ast.DeclType AstParsed.T)
rDeclType = ruleExpr
    [ varA @"actual_bind_var" <^> varA @"declconop" <^> varA @"actual_bind_var" <^> varA @"(':' type)?"
        <:> \(bindVar1 :* declconopE :* bindVar2 :* mayTyE :* HNil) ->
            [||case $$(declconopE) of { (declconop, spDeclconop) ->
                case $$(mayTyE) of { (mayTy, msMayTy) ->
                    Ast.DeclInfixType $$(bindVar1) declconop $$(bindVar2) mayTy
                        do AstParsed.sp
                            ($$(bindVar1), spDeclconop, $$(bindVar2) AstParsed.:<< msMayTy)
                }
            }||]
    , varA @"declcon" <^> varA @"bind_var*" <^> varA @"(':' type)?"
        <:> \(declconE :* bindVarsE :* mayTyE :* HNil) ->
            [||case $$(declconE) of { (declcon, spDeclcon) ->
                case $$(bindVarsE) of { (bindVars, msBindVars) ->
                    case $$(mayTyE) of { (mayTy, msMayTy) ->
                        Ast.DeclAppType declcon
                            do otoList bindVars
                            do mayTy
                            do AstParsed.sp
                                do spDeclcon AstParsed.:<< msBindVars AstParsed.:<< msMayTy
                    }
                }
            }||]
    ]

rConType :: RuleExpr (Ast.ConType AstParsed.T)
rConType = ruleExpr
    [ varA @"type_qualified" <^> varA @"conop_qualified" <^> varA @"type_qualified"
        <:> \(ty1 :* conopE :* ty2 :* HNil) ->
            [||case $$(conopE) of { (conop, spConop) ->
                Ast.ConInfixType $$(ty1) conop $$(ty2) do
                    AstParsed.sp ($$(ty1), spConop, $$(ty2))
            }||]
    , varA @"con_qualified" <^> varA @"type_app*"
        <:> \(conE :* typesE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                case $$(typesE) of { (types, msTypes) ->
                    Ast.ConAppType con
                        do otoList types
                        do AstParsed.sp do spCon AstParsed.:<< msTypes
                }
            }||]
    ]

rDeclVarExpr :: RuleExpr (Ast.DeclExpr AstParsed.T)
rDeclVarExpr = ruleExpr
    [ varA @"actual_bind_var" <^> varA @"declop" <^> varA @"actual_bind_var" <^> varA @"(':' type)?"
        <:> \(bindVar1 :* declopE :* bindVar2 :* mayTyE :* HNil) ->
            [||case $$(declopE) of { (declop, spDeclop) ->
                case $$(mayTyE) of { (mayTy, msMayTy) ->
                    Ast.DeclInfixExpr $$(bindVar1) declop $$(bindVar2) mayTy
                        do AstParsed.sp
                            ($$(bindVar1), spDeclop, $$(bindVar2) AstParsed.:<< msMayTy)
                }
            }||]
    , varA @"declvar" <^> varA @"bind_var*" <^> varA @"(':' type)?"
        <:> \(declvarE :* bindVarsE :* mayTyE :* HNil) ->
            [||case $$(declvarE) of { (declVar, spDeclVar) ->
                case $$(bindVarsE) of { (bindVars, msBindVars) ->
                    case $$(mayTyE) of { (mayTy, msMayTy) ->
                        Ast.DeclAppExpr declVar
                            do otoList bindVars
                            do mayTy
                            do AstParsed.sp
                                do spDeclVar AstParsed.:<< msBindVars AstParsed.:<< msMayTy
                    }
                }
            }||]
    ]


rType :: RuleExpr (Ast.TypeExpr AstParsed.T)
rType = ruleExpr
    [ varA @"type_infix"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeInfix :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeInfix = ruleExpr
    [ varA @"type_apps" <^> varA @"type_op" <^> varA @"type_infix"
        <:> \(typeApps :* typeOp :* typeInfix :* HNil) ->
            [||case $$(typeOp) of { (op, spOp) ->
                Ast.TypeInfix $$(typeApps) op $$(typeInfix) do
                    AstParsed.sp ($$(typeApps), spOp, $$(typeInfix))
            }||]
    , varA @"type_apps"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeOp :: RuleExpr (Ast.TypeExpr AstParsed.T, Spanned.Span)
rTypeOp = ruleExpr
    [ tokA @"`" <^> varA @"type_op_block" <^> tokA @"`"
        <:> \(_ :* ktick1 :* tyOp :* _ :* ktick2 :* HNil) ->
            [||
                ( $$(tyOp)
                , AstParsed.sp (lexToken $$(ktick1), $$(tyOp), lexToken $$(ktick2))
                )
            ||]
    , varA @"type_op_sym_qualified"
        <:> \(ty :* HNil) ->
            [||
                ( $$(ty)
                , AstParsed.sp $$(ty)
                )
            ||]
    ]

rTypeOpBlock :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeOpBlock = ruleExpr
    [ varA @"type_op_sym_qualified"
        <:> \(ty :* HNil) ->
            ty
    , varA @"type_apps"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeOpSymQualified :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeOpSymQualified = ruleExpr
    [ varA @"con_sym_ext"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.TypeCon con spCon
            }||]
    , varA @"var_sym_ext"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.TypeVar var spVar
            }||]
    ]

rTypeApps :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeApps = ruleExpr
    [ varA @"type_qualified" <^> varA @"type_app+"
        <:> \(ty :* typesE :* HNil) ->
            [||case $$(typesE) of { (types, spTypes) ->
                Ast.TypeApp $$(ty)
                    do otoList types
                    do AstParsed.sp ($$(ty), spTypes)
            }||]
    , varA @"type_qualified"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeApps1 :: RuleExpr (Bag.T (Ast.AppType AstParsed.T), Spanned.Span)
rTypeApps1 = ruleExpr
    [ varA @"type_app" <^> varA @"type_app*"
        <:> \(ty :* typesE :* HNil) ->
            [||case $$(typesE) of { (types, msTypes) ->
                ( cons $$(ty) types
                , AstParsed.sp do $$(ty) AstParsed.:<< msTypes
                )
            }||]
    ]

rTypeApps0 :: RuleExpr (Bag.T (Ast.AppType AstParsed.T), Maybe Spanned.Span)
rTypeApps0 = ruleExpr
    [ varA @"type_app" <^> varA @"type_app*"
        <:> \(ty :* typesE :* HNil) ->
            [||case $$(typesE) of { (types, msTypes) ->
                ( cons $$(ty) types
                , Just do AstParsed.sp do $$(ty) AstParsed.:<< msTypes
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rTypeApp :: RuleExpr (Ast.AppType AstParsed.T)
rTypeApp = ruleExpr
    [ tokA @"@" <^> varA @"type_qualified"
        <:> \(_ :* kat :* ty :* HNil) ->
            [||Ast.UnivAppType $$(ty) do
                AstParsed.sp (lexToken $$(kat), $$(ty))
            ||]
    , tokA @"#@" <^> varA @"type_block_body"
        <:> \(_ :* kbat :* body :* HNil) ->
            [||case $$(body) of { (ty, spBody) ->
                Ast.UnivAppType ty
                    do AstParsed.sp (lexToken $$(kbat), spBody)
            }||]
    , varA @"type_qualified"
        <:> \(ty :* HNil) ->
            [||Ast.AppType $$(ty) do
                AstParsed.sp $$(ty)
            ||]
    ]

rTypeQualified :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeQualified = ruleExpr
    [ varA @"type_block"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeQualifieds0 :: RuleExpr (Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)
rTypeQualifieds0 = ruleExpr
    [ varA @"type_qualified" <^> varA @"type_qualified*"
        <:> \(ty :* typesE :* HNil) ->
            [||case $$(typesE) of { (types, msTypes) ->
                ( cons $$(ty) types
                , Just do AstParsed.sp do $$(ty) AstParsed.:<< msTypes
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rTypeBlock :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeBlock = ruleExpr
    [ tokA @"^" <^> varA @"bind_var*" <^> tokA @"#>" <^> varA @"type"
        <:> \(_ :* kcaret :* bindVarsE :* _ :* karr :* ty :* HNil) ->
            [||case $$(bindVarsE) of { (bindVars, msBindVars) ->
                Ast.TypeForall
                    do otoList bindVars
                    do $$(ty)
                    do AstParsed.sp
                        ( lexToken $$(kcaret) AstParsed.:<< msBindVars
                        , lexToken $$(karr), $$(ty)
                        )
            }||]
    , tokA @"##" <^> varA @"type_block_body"
        <:> \(_ :* kblock :* body :* HNil) ->
            [||case $$(body) of { (ty, spBody) ->
                Ast.TypeAnn ty
                    do AstParsed.sp (lexToken $$(kblock), spBody)
            }||]
    , varA @"type_atomic"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeAtomic :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeAtomic = ruleExpr
    [ tokVarA @"(" <^> varA @"type" <^> tokA @":" <^> varA @"type" <^> tokVarA @")"
        <:> \(_ :* kop :* ty :* _ :* kcolon :* tySig :* _ :* kcp :* HNil) ->
            [||Ast.TypeSig $$(ty) $$(tySig)
                do AstParsed.sp ($$(kop), $$(ty), lexToken $$(kcolon), $$(tySig), $$(kcp))
            ||]
    , tokVarA @"(" <^> varA @"type" <^> tokVarA @")"
        <:> \(_ :* kop :* ty :* _ :* kcp :* HNil) ->
            [||Ast.TypeAnn $$(ty)
                do AstParsed.sp ($$(kop), $$(ty), $$(kcp))
            ||]
    , varA @"type_literal"
        <:> \(ty :* HNil) ->
            ty
    , varA @"con"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.TypeCon con spCon
            }||]
    , varA @"var"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.TypeVar var spVar
            }||]
    ]

rTypeLiteral :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeLiteral = ruleExpr
    [ varA @"literal"
        <:> \(lit :* HNil) ->
            [||Ast.TypeLit $$(lit) do
                AstParsed.sp $$(lit)
            ||]
    , tokVarA @"(" <^> varA @"type_tuple_items" <^> tokVarA @")"
        <:> \(_ :* kparenl :* typeTupleItems :* _ :* kparenr :* HNil) ->
            [||case $$(typeTupleItems) of { (items, spItems) ->
                Ast.TypeTuple items do
                    AstParsed.sp ($$(kparenl), spItems, $$(kparenr))
            }||]
    , tokVarA @"[" <^> varA @"type_array_items" <^> tokVarA @"]"
        <:> \(_ :* kbrackl :* typeArrayItems :* _ :* kbrackr :* HNil) ->
            [||case $$(typeArrayItems) of { (items, msItems) ->
                Ast.TypeArray items do
                    AstParsed.sp ($$(kbrackl), msItems AstParsed.:>> $$(kbrackr))
            }||]
    , tokVarA @"{" <^> varA @"type_simplrecord_items" <^> tokVarA @"}"
        <:> \(_ :* kbracel :* typeRecordItems :* _ :* kbracer :* HNil) ->
            [||case $$(typeRecordItems) of { (items, msItems) ->
                Ast.TypeRecord items do
                    AstParsed.sp ($$(kbracel), msItems AstParsed.:>> $$(kbracer))
            }||]
    ]

rTypeBlockBody :: RuleExpr (Ast.TypeExpr AstParsed.T, Spanned.Span)
rTypeBlockBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"type_block_item" <^> tokVarA @"}}"
        <:> \(_ :* kodbrace :* item :* _ :* kcdbrace :* HNil) ->
            [||case $$(item) of { (ty, spItem) ->
                ( ty
                , AstParsed.sp ($$(kodbrace), spItem, $$(kcdbrace))
                )
            }||]
    , tokVarA @"{" <^> varA @"type_block_item" <^> tokVarA @"}"
        <:> \(_ :* kodbrace :* item :* _ :* kcdbrace :* HNil) ->
            [||case $$(item) of { (ty, spItem) ->
                ( ty
                , AstParsed.sp ($$(kodbrace), spItem, $$(kcdbrace))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"type_block_item" <^> varA @"imp_bc"
        <:> \(_ :* _ :* item :* _ :* HNil) ->
            item
    ]

rTypeBlockItem :: RuleExpr (Ast.TypeExpr AstParsed.T, Spanned.Span)
rTypeBlockItem = ruleExpr
    [ varA @"lsemis?" <^> varA @"type" <^> varA @"lsemis?"
        <:> \(mlsemis1 :* ty :* mlsemis2 :* HNil) ->
            [||
                ( $$(ty)
                , AstParsed.sp do $$(mlsemis1) AstParsed.:>> $$(ty) AstParsed.:<< $$(mlsemis2)
                )
            ||]
    ]

rTypeTupleItems :: RuleExpr ([Ast.TypeExpr AstParsed.T], Spanned.Span)
rTypeTupleItems = ruleExpr
    [ tokA @"," <^> varA @"(type ',')+ type ','?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( otoList items
                , AstParsed.sp (lexToken $$(kcomma), spItems)
                )
            }||]
    , varA @"(type ',')+ type ','?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( otoList items
                , spItems
                )
            }||]
    ]

rTypesWithCommas2 :: RuleExpr (Bag.T (Ast.TypeExpr AstParsed.T), Spanned.Span)
rTypesWithCommas2 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')+ type ','?"
        <:> \(ty :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( cons $$(ty) items
                , AstParsed.sp ($$(ty), lexToken $$(kcomma), spItems)
                )
            }||]
    , varA @"type" <^> tokA @"," <^> varA @"type" <^> tokA @","
        <:> \(ty1 :* _ :* kcomma1 :* ty2 :* _ :* kcomma2 :* HNil) ->
            [||
                ( cons $$(ty1) do pure $$(ty2)
                , AstParsed.sp ($$(ty1), lexToken $$(kcomma1), $$(ty2), lexToken $$(kcomma2))
                )
            ||]
    , varA @"type" <^> tokA @"," <^> varA @"type"
        <:> \(ty1 :* _ :* kcomma1 :* ty2 :* HNil) ->
            [||
                ( cons $$(ty1) do pure $$(ty2)
                , AstParsed.sp ($$(ty1), lexToken $$(kcomma1), $$(ty2))
                )
            ||]
    ]

rTypeArrayItems :: RuleExpr ([Ast.TypeExpr AstParsed.T], Maybe Spanned.Span)
rTypeArrayItems = ruleExpr
    [ tokA @"," <^> varA @"(type ',')* type?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(type ',')* type?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rTypesWithCommas0 :: RuleExpr (Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)
rTypesWithCommas0 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')* type?"
        <:> \(ty :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(ty) items
                , Just do AstParsed.sp ($$(ty), lexToken $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"type"
        <:> \(ty :* HNil) ->
            [||
                ( Bag.singleton $$(ty)
                , Just do AstParsed.sp $$(ty)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rTypeSimpleRecordItems :: RuleExpr ([Ast.TypeRecordItem AstParsed.T], Maybe Spanned.Span)
rTypeSimpleRecordItems = ruleExpr
    [ tokA @"," <^> varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rTypeSimpleRecordItemsWithCommas0 :: RuleExpr (Bag.T (Ast.TypeRecordItem AstParsed.T), Maybe Spanned.Span)
rTypeSimpleRecordItemsWithCommas0 = ruleExpr
    [ varA @"type_simplrecord_item" <^> tokA @"," <^> varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(item :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp ($$(item), lexToken $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"type_simplrecord_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rTypeSimpleRecordItem :: RuleExpr (Ast.TypeRecordItem AstParsed.T)
rTypeSimpleRecordItem = ruleExpr
    [ varA @"declvar" <^> tokA @":" <^> varA @"type"
        <:> \(declvarE :* _ :* kcolon :* ty :* HNil) ->
            [||case $$(declvarE) of { (declvar, spDeclVar) ->
                Ast.TypeRecordItem declvar $$(ty) do
                    AstParsed.sp (spDeclVar, lexToken $$(kcolon), $$(ty))
            }||]
    ]


rSigItem :: RuleExpr (Ast.Decl AstParsed.T)
rSigItem = ruleExpr
    [ varA @"typesig_decl"
        <:> \(typeSigDecl :* HNil) ->
            typeSigDecl
    , varA @"valsig_decl"
        <:> \(valSigDecl :* HNil) ->
            valSigDecl
    , varA @"consig_decl"
        <:> \(conSigDecl :* HNil) ->
            conSigDecl
    ]


rExpr :: RuleExpr (Ast.Expr AstParsed.T)
rExpr = ruleExpr
    [ varA @"expr_infix" <^> tokA @":" <^> varA @"type"
        <:> \(expr :* _ :* kcolon :* ty :* HNil) ->
            [||Ast.ExprSig $$(expr) $$(ty) do
                AstParsed.sp ($$(expr), lexToken $$(kcolon), $$(ty))
            ||]
    , varA @"expr_infix"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprInfix :: RuleExpr (Ast.Expr AstParsed.T)
rExprInfix = ruleExpr
    [ varA @"expr_apps" <^> varA @"expr_op" <^> varA @"expr_infix"
        <:> \(expr1 :* opE :* expr2 :* HNil) ->
            [||case $$(opE) of { (op, spOp) ->
                Ast.ExprInfix $$(expr1) op $$(expr2) do
                    AstParsed.sp ($$(expr1), spOp, $$(expr2))
            }||]
    , varA @"expr_apps"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprOp :: RuleExpr (Ast.Expr AstParsed.T, Spanned.Span)
rExprOp = ruleExpr
    [ tokA @"`" <^> varA @"expr_op_block" <^> tokA @"`"
        <:> \(_ :* ktick1 :* expr :* _ :* ktick2 :* HNil) ->
            [||
                ( $$(expr)
                , AstParsed.sp (lexToken $$(ktick1), $$(expr), lexToken $$(ktick2))
                )
            ||]
    , varA @"expr_op_sym_qualified"
        <:> \(expr :* HNil) ->
            [||
                ( $$(expr)
                , AstParsed.sp $$(expr)
                )
            ||]
    ]

rExprOpBlock :: RuleExpr (Ast.Expr AstParsed.T)
rExprOpBlock = ruleExpr
    [ varA @"expr_op_sym_qualified"
        <:> \(expr :* HNil) ->
            expr
    , varA @"expr_apps"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprOpSymQualified :: RuleExpr (Ast.Expr AstParsed.T)
rExprOpSymQualified = ruleExpr
    [ varA @"con_sym_ext"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.ExprCon con spCon
            }||]
    , varA @"var_sym_ext"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.ExprVar var spVar
            }||]
    ]

rExprApps :: RuleExpr (Ast.Expr AstParsed.T)
rExprApps = ruleExpr
    [ varA @"expr_qualified" <^> varA @"expr_app+"
        <:> \(expr :* exprsE :* HNil) ->
            [||case $$(exprsE) of { (exprs, spExprs) ->
                Ast.ExprApp $$(expr)
                    do otoList exprs
                    do AstParsed.sp ($$(expr), spExprs)
            }||]
    , varA @"expr_qualified"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprApps1 :: RuleExpr (Bag.T (Ast.AppExpr AstParsed.T), Spanned.Span)
rExprApps1 = ruleExpr
    [ varA @"expr_app" <^> varA @"expr_app+"
        <:> \(expr :* exprsE :* HNil) ->
            [||case $$(exprsE) of { (exprs, spExprs) ->
                ( cons $$(expr) exprs
                , AstParsed.sp ($$(expr), spExprs)
                )
            }||]
    , varA @"expr_app"
        <:> \(expr :* HNil) ->
            [||
                ( Bag.singleton $$(expr)
                , AstParsed.sp $$(expr)
                )
            ||]
    ]

rExprApp :: RuleExpr (Ast.AppExpr AstParsed.T)
rExprApp = ruleExpr
    [ tokA @"@" <^> varA @"type_qualified"
        <:> \(_ :* kat :* ty :* HNil) ->
            [||Ast.UnivAppExpr $$(ty) do
                AstParsed.sp (lexToken $$(kat), $$(ty))
            ||]
    , tokA @"#@" <^> varA @"type_block_body"
        <:> \(_ :* kat :* body :* HNil) ->
            [||case $$(body) of { (ty, spBody) ->
                Ast.UnivAppExpr ty do
                    AstParsed.sp (lexToken $$(kat), spBody)
            }||]
    , varA @"expr_qualified"
        <:> \(expr :* HNil) ->
            [||Ast.AppExpr $$(expr) do
                AstParsed.sp $$(expr)
            ||]
    ]

rExprQualified :: RuleExpr (Ast.Expr AstParsed.T)
rExprQualified = ruleExpr
    [ varA @"expr_block"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprBlock :: RuleExpr (Ast.Expr AstParsed.T)
rExprBlock = ruleExpr
    [ tokA @"\\" <^> varA @"pat_atomic*" <^> varA @"guarded_alts"
        <:> \(_ :* kbackslash :* patsE :* altsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                case $$(altsE) of { (alts, spAlts) ->
                    let spAlt = AstParsed.sp do msPats AstParsed.:>> spAlts in
                    Ast.ExprLambda
                        [ Ast.CaseAlt
                            do otoList pats
                            alts spAlt
                        ]
                        do AstParsed.sp (lexToken $$(kbackslash), spAlt)
                }
            }||]
    , tokA @"#case" <^> varA @"case_alt_body"
        <:> \(_ :* kcase :* altsE :* HNil) ->
            [||case $$(altsE) of { (alts, msAlts) ->
                Ast.ExprLambda alts
                    do AstParsed.sp do lexToken $$(kcase) AstParsed.:<< msAlts
            }||]
    , tokA @"#letrec" <^> varA @"let_binds" <^> tokA @"#in" <^> varA @"expr"
        <:> \(_ :* kletrec :* bindsE :* _ :* kin :* expr :* HNil) ->
            [||case $$(bindsE) of { (binds, msBinds) ->
                Ast.ExprLetrec binds $$(expr) do
                    AstParsed.sp
                        ( lexToken $$(kletrec) AstParsed.:<< msBinds
                        , lexToken $$(kin), $$(expr)
                        )
            }||]
    , tokA @"#let" <^> varA @"let_binds" <^> tokA @"#in" <^> varA @"expr"
        <:> \(_ :* klet :* bindsE :* _ :* kin :* expr :* HNil) ->
            [||case $$(bindsE) of { (binds, msBinds) ->
                Ast.ExprLetrec binds $$(expr) do
                    AstParsed.sp
                        ( lexToken $$(klet) AstParsed.:<< msBinds
                        , lexToken $$(kin), $$(expr)
                        )
            }||]
    , tokA @"#match" <^> varA @"expr_match_items" <^> tokA @"#with" <^> varA @"case_alt_body"
        <:> \(_ :* kmatch :* exprsE :* _ :* kwith :* altsE :* HNil) ->
            [||case $$(exprsE) of { (exprs, msExprs) ->
                case $$(altsE) of { (alts, msAlts) ->
                    Ast.ExprMatch exprs alts do
                        AstParsed.sp
                            ( lexToken $$(kmatch) AstParsed.:<< msExprs
                            , lexToken $$(kwith) AstParsed.:<< msAlts
                            )
                }
            }||]
    , tokA @"#do" <^> varA @"do_body"
        <:> \(_ :* kdo :* doBody :* HNil) ->
            [||case $$(doBody) of { (stmts, expr, spBody) ->
                Ast.ExprDo stmts expr do
                    AstParsed.sp (lexToken $$(kdo), spBody)
            }||]
    , tokA @"##" <^> varA @"expr_block_body"
        <:> \(_ :* kblock :* body :* HNil) ->
            [||case $$(body) of { (expr, spBody) ->
                Ast.ExprAnn expr do
                    AstParsed.sp (lexToken $$(kblock), spBody)
            }||]
    , varA @"expr_atomic"
        <:> \(expr :* HNil) ->
            expr
    ]

rPatAtomics0 :: RuleExpr (Bag.T (Ast.Pat AstParsed.T), Maybe Spanned.Span)
rPatAtomics0 = ruleExpr
    [ varA @"pat_atomic" <^> varA @"pat_atomic*"
        <:> \(pat :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                ( cons $$(pat) pats
                , Just do AstParsed.sp do $$(pat) AstParsed.:<< msPats
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rExprAtomic :: RuleExpr (Ast.Expr AstParsed.T)
rExprAtomic = ruleExpr
    [ tokVarA @"(" <^> varA @"expr" <^> tokVarA @")"
        <:> \(_ :* kparenl :* expr :* _ :* kparenr :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                AstParsed.sp ($$(kparenl), $$(expr), $$(kparenr))
            ||]
    , varA @"expr_literal"
        <:> \(expr :* HNil) ->
            expr
    , varA @"con"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.ExprCon con spCon
            }||]
    , varA @"var"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.ExprVar var spVar
            }||]
    ]

rExprLiteral :: RuleExpr (Ast.Expr AstParsed.T)
rExprLiteral = ruleExpr
    [ varA @"literal"
        <:> \(lit :* HNil) ->
            [||Ast.ExprLit $$(lit) do
                AstParsed.sp $$(lit)
            ||]
    , varA @"expr_interp_string"
        <:> \(expr :* HNil) ->
            expr
    , tokVarA @"(" <^> varA @"expr_tuple_items" <^> tokVarA @")"
        <:> \(_ :* kparenl :* itemsE :* _ :* kparenr :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                Ast.ExprTuple items
                    do AstParsed.sp ($$(kparenl), spItems, $$(kparenr))
            }||]
    , tokVarA @"[" <^> varA @"expr_array_items" <^> tokVarA @"]"
        <:> \(_ :* kbrackl :* itemsE :* _ :* kbrackr :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.ExprArray items do
                    AstParsed.sp ($$(kbrackl), msItems AstParsed.:>> $$(kbrackr))
            }||]
    , tokVarA @"{" <^> varA @"expr_simplrecord_items" <^> tokVarA @"}"
        <:> \(_ :* kbracel :* itemsE :* _ :* kbracer :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.ExprRecord items do
                    AstParsed.sp ($$(kbracel), msItems AstParsed.:>> $$(kbracer))
            }||]
    ]

rExprBlockBody :: RuleExpr (Ast.Expr AstParsed.T, Spanned.Span)
rExprBlockBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"expr_block_item" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* item :* _ :* kdbracec :* HNil) ->
            [||case $$(item) of { (expr, spItem) ->
                ( expr
                , AstParsed.sp ($$(kdbraceo), spItem, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"expr_block_item" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* item :* _ :* kbracec :* HNil) ->
            [||case $$(item) of { (expr, spItem) ->
                ( expr
                , AstParsed.sp ($$(kbraceo), spItem, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"expr_block_item" <^> varA @"imp_bc"
        <:> \(_ :* _ :* item :* _ :* HNil) ->
            item
    ]

rExprBlockItem :: RuleExpr (Ast.Expr AstParsed.T, Spanned.Span)
rExprBlockItem = ruleExpr
    [ varA @"lsemis?" <^> varA @"expr" <^> varA @"lsemis?"
        <:> \(mlsemis1 :* expr :* mlsemis2 :* HNil) ->
            [||
                ( $$(expr)
                , AstParsed.sp do $$(mlsemis1) AstParsed.:>> $$(expr) AstParsed.:<< $$(mlsemis2)
                )
            ||]
    ]

rExprInterpString :: RuleExpr (Ast.Expr AstParsed.T)
rExprInterpString = ruleExpr
    [ tokA @"interp_string_without_interp"
        <:> \(_ :* lpart :* HNil) ->
            [||let litPart = interpStringLit do lexToken $$(lpart) in
                Ast.ExprInterpString
                    do pure litPart
                    do AstParsed.sp litPart
            ||]
    , tokVarA @"interp_string_start" <^> varA @"expr" <^> varA @"(interp_string_cont expr)* interp_string_end"
        <:> \(_ :* part :* expr :* partsE :* HNil) ->
            [||case $$(partsE) of { (parts, spParts) ->
                let exprPart = interpStringExpr $$(expr) in
                Ast.ExprInterpString
                    do $$(part) :| exprPart : otoList parts
                    do AstParsed.sp ($$(part), exprPart, spParts)
            }||]
    ]

rExprInterpStringContParts :: RuleExpr (Bag.T (Ast.InterpStringPart AstParsed.T), Spanned.Span)
rExprInterpStringContParts = ruleExpr
    [ tokVarA @"interp_string_cont" <^> varA @"expr" <^> varA @"(interp_string_cont expr)* interp_string_end"
        <:> \(_ :* part :* expr :* partsE :* HNil) ->
            [||case $$(partsE) of { (parts, spParts) ->
                let exprPart = interpStringExpr $$(expr)
                in
                    ( cons $$(part) do cons exprPart parts
                    , AstParsed.sp ($$(part), exprPart, spParts)
                    )
            }||]
    , tokVarA @"interp_string_end"
        <:> \(_ :* part :* HNil) ->
            [||
                ( Bag.singleton $$(part)
                , AstParsed.sp $$(part)
                )
            ||]
    ]

rExprMatchItems :: RuleExpr ([Ast.Expr AstParsed.T], Maybe Spanned.Span)
rExprMatchItems = ruleExpr
    [ tokA @"," <^> varA @"(expr ',')* expr?"
        <:> \(_ :* kcomma :* exprsE :* HNil) ->
            [||case $$(exprsE) of { (exprs, msExprs) ->
                ( otoList exprs
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msExprs
                )
            }||]
    , varA @"(expr ',')* expr?"
        <:> \(exprsE :* HNil) ->
            [||case $$(exprsE) of { (exprs, msExprs) ->
                ( otoList exprs
                , msExprs
                )
            }||]
    ]

rExprTupleItems :: RuleExpr ([Ast.Expr AstParsed.T], Spanned.Span)
rExprTupleItems = ruleExpr
    [ tokA @"," <^> varA @"(expr ',')+ expr ','?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( otoList items
                , AstParsed.sp (lexToken $$(kcomma), spItems)
                )
            }||]
    , varA @"(expr ',')+ expr ','?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( otoList items
                , spItems
                )
            }||]
    ]

rExprArrayItems :: RuleExpr ([Ast.Expr AstParsed.T], Maybe Spanned.Span)
rExprArrayItems = ruleExpr
    [ tokA @"," <^> varA @"(expr ',')* expr?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(expr ',')* expr?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rExprsWithCommas0 :: RuleExpr (Bag.T (Ast.Expr AstParsed.T), Maybe Spanned.Span)
rExprsWithCommas0 = ruleExpr
    [ varA @"expr" <^> tokA @"," <^> varA @"(expr ',')* expr?"
        <:> \(expr :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(expr) items
                , Just do AstParsed.sp ($$(expr), lexToken $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"expr"
        <:> \(expr :* HNil) ->
            [||
                ( Bag.singleton $$(expr)
                , Just do AstParsed.sp $$(expr)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rExprsWithCommas2 :: RuleExpr (Bag.T (Ast.Expr AstParsed.T), Spanned.Span)
rExprsWithCommas2 = ruleExpr
    [ varA @"expr" <^> tokA @"," <^> varA @"(expr ',')+ expr ','?"
        <:> \(expr :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( cons $$(expr) items
                , AstParsed.sp ($$(expr), lexToken $$(kcomma), spItems)
                )
            }||]
    , varA @"expr" <^> tokA @"," <^> varA @"expr" <^> tokA @","
        <:> \(expr1 :* _ :* kcomma1 :* expr2 :* _ :* kcomma2 :* HNil) ->
            [||
                ( cons $$(expr1) do pure $$(expr2)
                , AstParsed.sp ($$(expr1), lexToken $$(kcomma1), $$(expr2), lexToken $$(kcomma2))
                )
            ||]
    , varA @"expr" <^> tokA @"," <^> varA @"expr"
        <:> \(expr1 :* _ :* kcomma1 :* expr2 :* HNil) ->
            [||
                ( cons $$(expr1) do pure $$(expr2)
                , AstParsed.sp ($$(expr1), lexToken $$(kcomma1), $$(expr2))
                )
            ||]
    ]

rExprSimpleRecordItems :: RuleExpr ([Ast.ExprRecordItem AstParsed.T], Maybe Spanned.Span)
rExprSimpleRecordItems = ruleExpr
    [ tokA @"," <^> varA @"(expr_simplrecord_item ',')* expr_simplrecord_item?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(expr_simplrecord_item ',')* expr_simplrecord_item?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rExprSimpleRecordItemsWithCommas0 :: RuleExpr (Bag.T (Ast.ExprRecordItem AstParsed.T), Maybe Spanned.Span)
rExprSimpleRecordItemsWithCommas0 = ruleExpr
    [ varA @"expr_simplrecord_item" <^> tokA @"," <^> varA @"(expr_simplrecord_item ',')* expr_simplrecord_item?"
        <:> \(item :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp ($$(item), lexToken $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"expr_simplrecord_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rExprSimpleRecordItem :: RuleExpr (Ast.ExprRecordItem AstParsed.T)
rExprSimpleRecordItem = ruleExpr
    [ varA @"declvar" <^> tokA @"=" <^> varA @"expr"
        <:> \(declvar :* _ :* keq :* expr :* HNil) ->
            [||case $$(declvar) of { (declVar, spDeclVar) ->
                Ast.ExprRecordItem declVar $$(expr) do
                    AstParsed.sp (spDeclVar, lexToken $$(keq), $$(expr))
            }||]
    ]


rPat :: RuleExpr (Ast.Pat AstParsed.T)
rPat = ruleExpr
    [ varA @"pat_unit" <^> tokA @":" <^> varA @"type"
        <:> \(pat :* _ :* kcolon :* ty :* HNil) ->
            [||Ast.PatSig $$(pat) $$(ty) do
                AstParsed.sp ($$(pat), lexToken $$(kcolon), $$(ty))
            ||]
    , varA @"pat_unit"
        <:> \(pat :* HNil) ->
            pat
    ]

rPatsWithCommas0 :: RuleExpr (Bag.T (Ast.Pat AstParsed.T), Maybe Spanned.Span)
rPatsWithCommas0 = ruleExpr
    [ varA @"pat" <^> tokA @"," <^> varA @"(pat ',')* pat?"
        <:> \(pat :* _ :* kcomma :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                ( cons $$(pat) pats
                , Just do AstParsed.sp ($$(pat), lexToken $$(kcomma) AstParsed.:<< msPats)
                )
            }||]
    , varA @"pat"
        <:> \(pat :* HNil) ->
            [||
                ( Bag.singleton $$(pat)
                , Just do AstParsed.sp $$(pat)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rPatsWithCommas2 :: RuleExpr (Bag.T (Ast.Pat AstParsed.T), Spanned.Span)
rPatsWithCommas2 = ruleExpr
    [ varA @"pat" <^> tokA @"," <^> varA @"(pat ',')+ pat ','?"
        <:> \(item :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( cons $$(item) items
                , AstParsed.sp
                    ( $$(item), lexToken $$(kcomma), spItems )
                )
            }||]
    , varA @"pat" <^> tokA @"," <^> varA @"pat" <^> tokA @","
        <:> \(item1 :* _ :* kcomma1 :* item2 :* _ :* kcomma2 :* HNil) ->
            [||
                ( cons $$(item1) do pure $$(item2)
                , AstParsed.sp
                    ( $$(item1), lexToken $$(kcomma1)
                    , $$(item2), lexToken $$(kcomma2)
                    )
                )
            ||]
    , varA @"pat" <^> tokA @"," <^> varA @"pat"
        <:> \(item1 :* _ :* kcomma :* item2 :* HNil) ->
            [||
                ( cons $$(item1) do pure $$(item2)
                , AstParsed.sp
                    ( $$(item1), lexToken $$(kcomma)
                    , $$(item2)
                    )
                )
            ||]
    ]

rPatUnit :: RuleExpr (Ast.Pat AstParsed.T)
rPatUnit = ruleExpr
    [ tokA @"|" <^> varA @"(pat_infix '|')* pat_infix '|'?"
        <:> \(_ :* kor :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, spPats) ->
                Ast.PatOr
                    do otoList pats
                    do AstParsed.sp (lexToken $$(kor), spPats)
            }||]
    ]

rPatInfixesWithBars1 :: RuleExpr (Bag.T (Ast.Pat AstParsed.T), Spanned.Span)
rPatInfixesWithBars1 = ruleExpr
    [ varA @"pat_infix" <^> tokA @"|" <^> varA @"(pat_infix '|')* pat_infix '|'?"
        <:> \(pat :* _ :* kor :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, spPats) ->
                ( cons $$(pat) pats
                , AstParsed.sp ($$(pat), lexToken $$(kor), spPats)
                )
            }||]
    , varA @"pat_infix" <^> tokA @"|"
        <:> \(pat :* _ :* kor :* HNil) ->
            [||
                ( Bag.singleton $$(pat)
                , AstParsed.sp ($$(pat), lexToken $$(kor))
                )
            ||]
    , varA @"pat_infix"
        <:> \(pat :* HNil) ->
            [||
                ( Bag.singleton $$(pat)
                , AstParsed.sp $$(pat)
                )
            ||]
    ]

rPatInfix :: RuleExpr (Ast.Pat AstParsed.T)
rPatInfix = ruleExpr
    [ varA @"pat_apps" <^> varA @"pat_op" <^> varA @"pat_infix"
        <:> \(pat1 :* opE :* pat2 :* HNil) ->
            [||case $$(opE) of { (op, spOp) ->
                Ast.PatInfix $$(pat1) op $$(pat2) do
                    AstParsed.sp ($$(pat1), spOp, $$(pat2))
            }||]
    , varA @"pat_apps"
        <:> \(pat :* HNil) ->
            pat
    ]

rPatOp :: RuleExpr (Ast.PatOp AstParsed.T, Spanned.Span)
rPatOp = ruleExpr
    [ tokA @"`" <^> varA @"pat_op_block" <^> tokA @"`"
        <:> \(_ :* kcaret1 :* patOp :* _ :* kcaret2 :* HNil) ->
            [||
                ( $$(patOp)
                , AstParsed.sp (lexToken $$(kcaret1), $$(patOp), lexToken $$(kcaret2))
                )
            ||]
    , varA @"pat_op_sym_qualified"
        <:> \(patOp :* HNil) ->
            [||
                ( $$(patOp)
                , AstParsed.sp $$(patOp)
                )
            ||]
    ]

rPatOpBlock :: RuleExpr (Ast.PatOp AstParsed.T)
rPatOpBlock = ruleExpr
    [ varA @"pat_op_sym_qualified"
        <:> \(patOp :* HNil) ->
            patOp
    , varA @"con_qualified" <^> varA @"pat_app*"
        <:> \(conE :* patsE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                case $$(patsE) of { (pats, msPats) ->
                    Ast.PatOpConApp con
                        do otoList pats
                        do AstParsed.sp do spCon AstParsed.:<< msPats
                }
            }||]
    ]

rPatOpSymQualified :: RuleExpr (Ast.PatOp AstParsed.T)
rPatOpSymQualified = ruleExpr
    [ varA @"con_sym_ext"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.PatOpConApp
                    do con
                    do []
                    do AstParsed.sp spCon
            }||]
    ]

rPatApps :: RuleExpr (Ast.Pat AstParsed.T)
rPatApps = ruleExpr
    [ varA @"con_qualified" <^> varA @"pat_app*"
        <:> \(conE :* patsE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                case $$(patsE) of { (pats, msPats) ->
                    Ast.PatConApp con
                        do otoList pats
                        do AstParsed.sp do spCon AstParsed.:<< msPats
                }
            }||]
    , varA @"pat_qualified" <^> varA @"pat_univ_app*"
        <:> \(pat :* typesE :* HNil) ->
            [||case $$(typesE) of { (types, msTypes) ->
                Ast.PatUnivApp $$(pat)
                    do otoList types
                    do AstParsed.sp do $$(pat) AstParsed.:<< msTypes
            }||]
    ]

rPatApp :: RuleExpr (Ast.AppPat AstParsed.T)
rPatApp = ruleExpr
    [ varA @"pat_univ_app"
        <:> \(tyAppE :* HNil) ->
            [||case $$(tyAppE) of { (ty, spTyApp) ->
                Ast.UnivAppPat ty spTyApp
            }||]
    , varA @"pat_qualified"
        <:> \(pat :* HNil) ->
            [||Ast.AppPat $$(pat) do AstParsed.sp $$(pat)||]
    ]

rPatApps0 :: RuleExpr (Bag.T (Ast.AppPat AstParsed.T), Maybe Spanned.Span)
rPatApps0 = ruleExpr
    [ varA @"pat_app" <^> varA @"pat_app*"
        <:> \(pat :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                ( cons $$(pat) pats
                , Just do AstParsed.sp do $$(pat) AstParsed.:<< msPats
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rPatUnivApp :: RuleExpr (Ast.TypeExpr AstParsed.T, Spanned.Span)
rPatUnivApp = ruleExpr
    [ tokA @"@" <^> varA @"type_qualified"
        <:> \(_ :* kat :* ty :* HNil) ->
            [||
                ( $$(ty)
                , AstParsed.sp (lexToken $$(kat), $$(ty))
                )
            ||]
    , tokA @"#@" <^> varA @"type_block_body"
        <:> \(_ :* kat :* body :* HNil) ->
            [||case $$(body) of { (ty, spTy) ->
                ( ty
                , AstParsed.sp (lexToken $$(kat), spTy)
                )
            }||]
    ]

rPatUnivApps0 :: RuleExpr (Bag.T (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)
rPatUnivApps0 = ruleExpr
    [ varA @"pat_univ_app" <^> varA @"pat_univ_app*"
        <:> \(itemE :* itemsE :* HNil) ->
            [||case $$(itemE) of { (item, spItem) ->
                case $$(itemsE) of { (items, msItems) ->
                    ( cons item items
                    , Just do AstParsed.sp do spItem AstParsed.:<< msItems
                    )
                }
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rPatQualified :: RuleExpr (Ast.Pat AstParsed.T)
rPatQualified = ruleExpr
    [ varA @"pat_block"
        <:> \(pat :* HNil) ->
            pat
    ]

rPatBlock :: RuleExpr (Ast.Pat AstParsed.T)
rPatBlock = ruleExpr
    [ tokA @"##" <^> varA @"pat_block_body"
        <:> \(_ :* kblock :* body :* HNil) ->
            [||case $$(body) of { (pat, spBody) ->
                Ast.PatAnn pat
                    do AstParsed.sp (lexToken $$(kblock), spBody)
            }||]
    , varA @"pat_atomic"
        <:> \(pat :* HNil) ->
            pat
    ]

rPatAtomic :: RuleExpr (Ast.Pat AstParsed.T)
rPatAtomic = ruleExpr
    [ tokVarA @"(" <^> varA @"pat" <^> tokVarA @")"
        <:> \(_ :* kop :* pat :* _ :* kcp :* HNil) ->
            [||Ast.PatAnn $$(pat)
                do AstParsed.sp ($$(kop), $$(pat), $$(kcp))
            ||]
    , varA @"pat_literal"
        <:> \(pat :* HNil) ->
            pat
    , varA @"con"
        <:> \(conE :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                Ast.PatConApp con [] spCon
            }||]
    , varA @"var"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.PatVar var spVar
            }||]
    ]

rPatLiteral :: RuleExpr (Ast.Pat AstParsed.T)
rPatLiteral = ruleExpr
    [ varA @"literal"
        <:> \(lit :* HNil) ->
            [||Ast.PatLit $$(lit)
                do AstParsed.sp $$(lit)
            ||]
    , tokVarA @"(" <^> varA @"pat_tuple_items" <^> tokVarA @")"
        <:> \(_ :* kop :* itemsE :* _ :* kcp :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                Ast.PatTuple items
                    do AstParsed.sp ($$(kop), spItems, $$(kcp))
            }||]
    , tokVarA @"[" <^> varA @"pat_array_items" <^> tokVarA @"]"
        <:> \(_ :* kop :* itemsE :* _ :* kcp :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.PatArray items
                    do AstParsed.sp ($$(kop) AstParsed.:<< msItems, $$(kcp))
            }||]
    , tokVarA @"{" <^> varA @"pat_simplrecord_items" <^> tokVarA @"}"
        <:> \(_ :* kop :* itemsE :* _ :* kcp :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.PatRecord items
                    do AstParsed.sp ($$(kop) AstParsed.:<< msItems, $$(kcp))
            }||]
    ]

rPatBlockBody :: RuleExpr (Ast.Pat AstParsed.T, Spanned.Span)
rPatBlockBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"pat_block_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( items
                , AstParsed.sp ($$(kdbraceo), spItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"pat_block_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (items, spItems) ->
                ( items
                , AstParsed.sp ($$(kbraceo), spItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"pat_block_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rPatBlockItems :: RuleExpr (Ast.Pat AstParsed.T, Spanned.Span)
rPatBlockItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"pat" <^> varA @"lsemis?"
        <:> \(mlsemis1 :* pat :* mlsemis2 :* HNil) ->
            [||
                ( $$(pat)
                , AstParsed.sp do $$(mlsemis1) AstParsed.:>> $$(pat) AstParsed.:<< $$(mlsemis2)
                )
            ||]
    ]

rPatTupleItems :: RuleExpr ([Ast.Pat AstParsed.T], Spanned.Span)
rPatTupleItems = ruleExpr
    [ tokA @"," <^> varA @"(pat ',')+ pat ','?"
        <:> \(_ :* kcomma :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, spPats) ->
                ( otoList pats
                , AstParsed.sp (lexToken $$(kcomma), spPats)
                )
            }||]
    , varA @"(pat ',')+ pat ','?"
        <:> \(patsE :* HNil) ->
            [||case $$(patsE) of { (pats, spPats) ->
                ( otoList pats
                , spPats
                )
            }||]
    ]

rPatArrayItems :: RuleExpr ([Ast.Pat AstParsed.T], Maybe Spanned.Span)
rPatArrayItems = ruleExpr
    [ tokA @"," <^> varA @"(pat ',')* pat?"
        <:> \(_ :* kcomma :* patsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                ( otoList pats
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msPats
                )
            }||]
    , varA @"(pat ',')* pat?"
        <:> \(patsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                ( otoList pats
                , msPats
                )
            }||]
    ]

rPatSimpleRecordItems :: RuleExpr ([Ast.PatRecordItem AstParsed.T], Maybe Spanned.Span)
rPatSimpleRecordItems = ruleExpr
    [ tokA @"," <^> varA @"(pat_simplrecord_item ',')* pat_simplrecord_item?"
        <:> \(_ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , Just do AstParsed.sp do lexToken $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(pat_simplrecord_item ',')* pat_simplrecord_item?"
        <:> \(itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , msItems
                )
            }||]
    ]

rPatSimpleRecordItem :: RuleExpr (Ast.PatRecordItem AstParsed.T)
rPatSimpleRecordItem = ruleExpr
    [ varA @"declvar" <^> tokA @"=" <^> varA @"pat"
        <:> \(varE :* _ :* keq :* pat :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                Ast.PatRecordItem var $$(pat)
                    do AstParsed.sp (spVar, lexToken $$(keq), $$(pat))
            }||]
    ]

rPatSimpleRecordItemsWithCommas0 :: RuleExpr (Bag.T (Ast.PatRecordItem AstParsed.T), Maybe Spanned.Span)
rPatSimpleRecordItemsWithCommas0 = ruleExpr
    [ varA @"pat_simplrecord_item" <^> tokA @"," <^> varA @"(pat_simplrecord_item ',')* pat_simplrecord_item?"
        <:> \(item :* _ :* kcomma :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp ($$(item), lexToken $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"pat_simplrecord_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]


rLetBinds :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rLetBinds = ruleExpr
    [ tokVarA @"{{" <^> varA @"let_bind_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kdbraceo) AstParsed.:<< msItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"let_bind_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kbraceo) AstParsed.:<< msItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"let_bind_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rLetBindItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rLetBindItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(let_bind_item lsemis)* let_bind_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rLetBindItemsWithSemis0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rLetBindItemsWithSemis0 = ruleExpr
    [ varA @"let_bind_item" <^> varA @"lsemis" <^> varA @"(let_bind_item lsemis)* let_bind_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just
                    do AstParsed.sp
                        do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"let_bind_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rLetBindItem :: RuleExpr (Ast.Decl AstParsed.T)
rLetBindItem = ruleExpr
    [ varA @"type_decl"
        <:> \(item :* HNil) ->
            item
    , varA @"data_decl"
        <:> \(item :* HNil) ->
            item
    , varA @"val_bind"
        <:> \(item :* HNil) ->
            item
    , varA @"sig_item"
        <:> \(item :* HNil) ->
            item
    ]


rCaseAltBody :: RuleExpr ([Ast.CaseAlt AstParsed.T], Maybe Spanned.Span)
rCaseAltBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"case_alt_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kdbraceo) AstParsed.:<< msItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"case_alt_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kbraceo) AstParsed.:<< msItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"case_alt_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rCaseAltItems :: RuleExpr ([Ast.CaseAlt AstParsed.T], Maybe Spanned.Span)
rCaseAltItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(case_alt_item lsemis)* case_alt_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rCaseAltItemsWithSemis0 :: RuleExpr (Bag.T (Ast.CaseAlt AstParsed.T), Maybe Spanned.Span)
rCaseAltItemsWithSemis0 = ruleExpr
    [ varA @"case_alt_item" <^> varA @"lsemis" <^> varA @"(case_alt_item lsemis)* case_alt_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"case_alt_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rCaseAltItem :: RuleExpr (Ast.CaseAlt AstParsed.T)
rCaseAltItem = ruleExpr
    [ tokA @"," <^> varA @"(pat ',')* pat?" <^> varA @"guarded_alts"
        <:> \(_ :* kcomma :* patsE :* altsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                case $$(altsE) of { (alts, spAlts) ->
                    Ast.CaseAlt
                        do otoList pats
                        do alts
                        do AstParsed.sp (lexToken $$(kcomma) AstParsed.:<< msPats, spAlts)
                }
            }||]
    , varA @"(pat ',')* pat?" <^> varA @"guarded_alts"
        <:> \(patsE :* altsE :* HNil) ->
            [||case $$(patsE) of { (pats, msPats) ->
                case $$(altsE) of { (alts, spAlts) ->
                    Ast.CaseAlt
                        do otoList pats
                        do alts
                        do AstParsed.sp do msPats AstParsed.:>> spAlts
                }
            }||]
    ]

rGuardedAlts :: RuleExpr ([Ast.GuardedAlt AstParsed.T], Spanned.Span)
rGuardedAlts = ruleExpr
    [ tokA @"#>" <^> varA @"expr"
        <:> \(_ :* karr :* expr :* HNil) ->
            [||
                ( pure
                    do Ast.GuardedAlt Nothing $$(expr)
                        do AstParsed.sp $$(expr)
                , AstParsed.sp (lexToken $$(karr), $$(expr))
                )
            ||]
    , tokA @"#when" <^> varA @"guarded_alt_body"
        <:> \(_ :* kwhen :* body :* HNil) ->
            [||case $$(body) of { (items, msItems) ->
                ( items
                , AstParsed.sp do lexToken $$(kwhen) AstParsed.:<< msItems
                )
            }||]
    ]

rGuardedAltBody :: RuleExpr ([Ast.GuardedAlt AstParsed.T], Maybe Spanned.Span)
rGuardedAltBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"guarded_alt_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kdbraceo) AstParsed.:<< msItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"guarded_alt_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( items
                , Just do AstParsed.sp ($$(kbraceo) AstParsed.:<< msItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"guarded_alt_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rGuardedAltItems :: RuleExpr ([Ast.GuardedAlt AstParsed.T], Maybe Spanned.Span)
rGuardedAltItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(guarded_alt_item lsemis)* guarded_alt_item?"
        <:> \(mayLsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( otoList items
                , AstParsed.maySp ($$(mayLsemis), msItems)
                )
            }||]
    ]

rGuardedAltItemsWithSemis0 :: RuleExpr (Bag.T (Ast.GuardedAlt AstParsed.T), Maybe Spanned.Span)
rGuardedAltItemsWithSemis0 = ruleExpr
    [ varA @"guarded_alt_item" <^> varA @"lsemis" <^> varA @"(guarded_alt_item lsemis)* guarded_alt_item?"
        <:> \(item :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< $$(lsemis) AstParsed.:<< msItems
                )
            }||]
    , varA @"guarded_alt_item"
        <:> \(item :* HNil) ->
            [||
                ( Bag.singleton $$(item)
                , Just do AstParsed.sp $$(item)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rGuardedAltItem :: RuleExpr (Ast.GuardedAlt AstParsed.T)
rGuardedAltItem = ruleExpr
    [ varA @"guard_qual" <^> tokA @"#>" <^> varA @"expr"
        <:> \(qual :* _ :* karr :* expr :* HNil) ->
            [||Ast.GuardedAlt
                do Just $$(qual)
                do $$(expr)
                do AstParsed.sp ($$(qual), lexToken $$(karr), $$(expr))
            ||]
    ]

rGuardQual :: RuleExpr (Ast.Expr AstParsed.T)
rGuardQual = ruleExpr
    [ varA @"expr"
        <:> \(expr :* HNil) ->
            expr
    ]


rDoBody :: RuleExpr ([Ast.DoStmt AstParsed.T], Ast.Expr AstParsed.T, Spanned.Span)
rDoBody = ruleExpr
    [ tokVarA @"{{" <^> varA @"do_stmt_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (stmts, expr, spItems) ->
                ( stmts, expr
                , AstParsed.sp ($$(kdbraceo), spItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"do_stmt_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (stmts, expr, spItems) ->
                ( stmts, expr
                , AstParsed.sp ($$(kbraceo), spItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"do_stmt_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rDoStmtItems :: RuleExpr ([Ast.DoStmt AstParsed.T], Ast.Expr AstParsed.T, Spanned.Span)
rDoStmtItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(do_stmt_item lsemis)* do_yield_item lsemis?"
        <:> \(mayLsemis1 :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (stmts, expr, spItems) ->
                ( otoList stmts
                , expr
                , AstParsed.sp do $$(mayLsemis1) AstParsed.:>> spItems
                )
            }||]
    ]

rDoStmtItemsWithSemis1 :: RuleExpr (Bag.T (Ast.DoStmt AstParsed.T), Ast.Expr AstParsed.T, Spanned.Span)
rDoStmtItemsWithSemis1 = ruleExpr
    [ varA @"do_stmt_item" <^> varA @"lsemis" <^> varA @"(do_stmt_item lsemis)* do_yield_item lsemis?"
        <:> \(stmt :* lsemis :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (stmts, expr, spItems) ->
                ( cons $$(stmt) stmts
                , expr
                , AstParsed.sp ($$(stmt) AstParsed.:<< $$(lsemis), spItems)
                )
            }||]
    , varA @"do_yield_item" <^> varA @"lsemis?"
        <:> \(yieldItem :* mayLsemis :* HNil) ->
            [||case $$(yieldItem) of { (expr, spItem) ->
                ( Bag.empty
                , expr
                , AstParsed.sp do spItem AstParsed.:<< $$(mayLsemis)
                )
            }||]
    ]

rDoStmtItem :: RuleExpr (Ast.DoStmt AstParsed.T)
rDoStmtItem = ruleExpr
    [ tokA @"#letrec" <^> varA @"let_binds"
        <:> \(_ :* kletrec :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                Ast.DoStmtLetrec items
                    do AstParsed.sp do lexToken $$(kletrec) AstParsed.:<< msItems
            }||]
    , varA @"pat" <^> tokA @"#<" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(pat :* _ :* karr :* expr :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DoStmtMonBind $$(pat) $$(expr) whereItems
                    do AstParsed.sp
                        ( $$(pat), lexToken $$(karr), $$(expr)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , varA @"pat" <^> tokA @"#<" <^> varA @"expr"
        <:> \(pat :* _ :* karr :* expr :* HNil) ->
            [||Ast.DoStmtMonBind $$(pat) $$(expr) []
                do AstParsed.sp ($$(pat), lexToken $$(karr), $$(expr))
            ||]
    , varA @"pat" <^> tokA @"=" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(pat :* _ :* keq :* expr :* _ :* kwhere :* whereBody :* HNil) ->
            [||case $$(whereBody) of { (whereItems, msWhereItems) ->
                Ast.DoStmtBind $$(pat) $$(expr) whereItems
                    do AstParsed.sp
                        ( $$(pat), lexToken $$(keq), $$(expr)
                        , lexToken $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , varA @"pat" <^> tokA @"=" <^> varA @"expr"
        <:> \(pat :* _ :* keq :* expr :* HNil) ->
            [||Ast.DoStmtBind $$(pat) $$(expr) []
                do AstParsed.sp ($$(pat), lexToken $$(keq), $$(expr))
            ||]
    ]

rDoYieldItem :: RuleExpr (Ast.Expr AstParsed.T, Spanned.Span)
rDoYieldItem = ruleExpr
    [ tokA @"#yield" <^> varA @"expr"
        <:> \(_ :* kyield :* expr :* HNil) ->
            [||
                ( $$(expr)
                , AstParsed.sp (lexToken $$(kyield), $$(expr))
                )
            ||]
    ]


rBindVar :: RuleExpr (Ast.BindVar AstParsed.T)
rBindVar = ruleExpr
    [ tokA @"#@" <^> varA @"block_bind_var"
        <:> \(_ :* kat :* bindVar :* HNil) ->
            [||case $$(bindVar) of { (n, mayTy, spBindVar) ->
                Ast.BindVar n mayTy
                    do AstParsed.sp (lexToken $$(kat), spBindVar)
            }||]
    , tokA @"@" <^> varA @"simple_bind_var"
        <:> \(_ :* kat :* bindVar :* HNil) ->
            [||case $$(bindVar) of { (n, mayTy, spBindVar) ->
                Ast.BindVar n mayTy
                    do AstParsed.sp (lexToken $$(kat), spBindVar)
            }||]
    , varA @"actual_bind_var"
        <:> \(bindVar :* HNil) ->
            bindVar
    ]

rBindVars0 :: RuleExpr (Bag.T (Ast.BindVar AstParsed.T), Maybe Spanned.Span)
rBindVars0 = ruleExpr
    [ varA @"bind_var" <^> varA @"bind_var*"
        <:> \(item :* itemsE :* HNil) ->
            [||case $$(itemsE) of { (items, msItems) ->
                ( cons $$(item) items
                , Just do AstParsed.sp do $$(item) AstParsed.:<< msItems
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( Bag.empty
                , Nothing
                )
            ||]
    ]

rActualBindVar :: RuleExpr (Ast.BindVar AstParsed.T)
rActualBindVar = ruleExpr
    [ tokA @"##" <^> varA @"block_bind_var"
        <:> \(_ :* kblock :* bindVar :* HNil) ->
            [||case $$(bindVar) of { (n, mayTy, spBindVar) ->
                Ast.BindVar n mayTy
                    do AstParsed.sp (lexToken $$(kblock), spBindVar)
            }||]
    , varA @"simple_bind_var"
        <:> \(bindVar :* HNil) ->
            [||case $$(bindVar) of { (n, mayTy, spBindVar) ->
                Ast.BindVar n mayTy spBindVar
            }||]
    ]

rSimpleBindVar :: RuleExpr (Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)
rSimpleBindVar = ruleExpr
    [ tokVarA @"(" <^> varA @"block_bind_var_item" <^> tokVarA @")"
        <:> \(_ :* kop :* item :* _ :* kcp :* HNil) ->
            [||case $$(item) of { (n, mayTy, spItem) ->
                ( n, mayTy
                , AstParsed.sp ($$(kop), spItem, $$(kcp))
                )
            }||]
    , varA @"var_id_ext"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                ( var, Nothing, spVar )
            }||]
    ]

rBlockBindVar :: RuleExpr (Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)
rBlockBindVar = ruleExpr
    [ tokVarA @"{{" <^> varA @"block_bind_var_items" <^> tokVarA @"}}"
        <:> \(_ :* kdbraceo :* itemsE :* _ :* kdbracec :* HNil) ->
            [||case $$(itemsE) of { (n, mayTy, spItems) ->
                ( n, mayTy
                , AstParsed.sp ($$(kdbraceo), spItems, $$(kdbracec))
                )
            }||]
    , tokVarA @"{" <^> varA @"block_bind_var_items" <^> tokVarA @"}"
        <:> \(_ :* kbraceo :* itemsE :* _ :* kbracec :* HNil) ->
            [||case $$(itemsE) of { (n, mayTy, spItems) ->
                ( n, mayTy
                , AstParsed.sp ($$(kbraceo), spItems, $$(kbracec))
                )
            }||]
    , tokVarA @"{n}" <^> varA @"block_bind_var_items" <^> varA @"imp_bc"
        <:> \(_ :* _ :* items :* _ :* HNil) ->
            items
    ]

rBlockBindVarItems :: RuleExpr (Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)
rBlockBindVarItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"block_bind_var_item" <^> varA @"lsemis?"
        <:> \(mayLsemis1 :* itemE :* mayLsemis2 :* HNil) ->
            [||case $$(itemE) of { (n, mayTy, spItem) ->
                ( n, mayTy
                , AstParsed.sp do $$(mayLsemis1) AstParsed.:>> spItem AstParsed.:<< $$(mayLsemis2)
                )
            }||]
    ]

rBlockBindVarItem :: RuleExpr (Ast.Name, Maybe (Ast.TypeExpr AstParsed.T), Spanned.Span)
rBlockBindVarItem = ruleExpr
    [ varA @"var_id_ext" <^> tokA @":" <^> varA @"type"
        <:> \(varE :* _ :* kcolon :* ty :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                ( var, Just $$(ty)
                , AstParsed.sp (spVar, lexToken $$(kcolon), $$(ty))
                )
            }||]
    , varA @"var_id_ext"
        <:> \(varE :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                ( var, Nothing
                , spVar
                )
            }||]
    ]

rConQualified :: RuleExpr (Ast.Name, Spanned.Span)
rConQualified = ruleExpr
    [ varA @"con"
        <:> \(con :* HNil) ->
            con
    ]

rConOpQualified :: RuleExpr (Ast.Name, Spanned.Span)
rConOpQualified = ruleExpr
    [ varA @"conop"
        <:> \(conOp :* HNil) ->
            conOp
    ]

rCon :: RuleExpr (Ast.Name, Spanned.Span)
rCon = ruleExpr
    [ tokVarA @"(" <^> varA @"con_sym_ext" <^> tokVarA @")"
        <:> \(_ :* kop :* conE :* _ :* kcp :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                ( con
                , AstParsed.sp ($$(kop), spCon, $$(kcp))
                )
            }||]
    , tokVarA @"(" <^> varA @"con_id_ext" <^> tokVarA @")"
        <:> \(_ :* kop :* conE :* _ :* kcp :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                ( con
                , AstParsed.sp ($$(kop), spCon, $$(kcp))
                )
            }||]
    , varA @"con_id_ext"
        <:> \(con :* HNil) ->
            con
    ]

rConOp :: RuleExpr (Ast.Name, Spanned.Span)
rConOp = ruleExpr
    [ tokA @"`" <^> varA @"con_sym_ext" <^> tokA @"`"
        <:> \(_ :* kt1 :* conE :* _ :* kt2 :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                ( con
                , AstParsed.sp (lexToken $$(kt1), spCon, lexToken $$(kt2))
                )
            }||]
    , tokA @"`" <^> varA @"con_id_ext" <^> tokA @"`"
        <:> \(_ :* kt1 :* conE :* _ :* kt2 :* HNil) ->
            [||case $$(conE) of { (con, spCon) ->
                ( con
                , AstParsed.sp (lexToken $$(kt1), spCon, lexToken $$(kt2))
                )
            }||]
    , varA @"con_sym_ext"
        <:> \(con :* HNil) ->
            con
    ]

rVar :: RuleExpr (Ast.Name, Spanned.Span)
rVar = ruleExpr
    [ tokVarA @"(" <^> varA @"var_sym_ext" <^> tokVarA @")"
        <:> \(_ :* kop :* varE :* _ :* kcp :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                ( var
                , AstParsed.sp ($$(kop), spVar, $$(kcp))
                )
            }||]
    , tokVarA @"(" <^> varA @"var_id_ext" <^> tokVarA @")"
        <:> \(_ :* kop :* varE :* _ :* kcp :* HNil) ->
            [||case $$(varE) of { (var, spVar) ->
                ( var
                , AstParsed.sp ($$(kop), spVar, $$(kcp))
                )
            }||]
    , varA @"var_id_ext"
        <:> \(var :* HNil) ->
            var
    ]

rConIdExt :: RuleExpr (Ast.Name, Spanned.Span)
rConIdExt = ruleExpr
    [ tokA @"(" <^> tokA @")"
        <:> \(_ :* kop :* _ :* kcp :* HNil) ->
            [||
                ( Ast.primNameUnit
                , AstParsed.sp (lexToken $$(kop), lexToken $$(kcp))
                )
            ||]
    , tokA @"con_id"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConId n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a con_id token."
            ||]
    ]

rConSymExt :: RuleExpr (Ast.Name, Spanned.Span)
rConSymExt = ruleExpr
    [ tokA @"#->"
        <:> \(_ :* karr :* HNil) ->
            [||
                ( Ast.primNameArrow
                , AstParsed.sp do lexToken $$(karr)
                )
            ||]
    , tokA @"#=>"
        <:> \(_ :* karr :* HNil) ->
            [||
                ( Ast.primNameDerive
                , AstParsed.sp do lexToken $$(karr)
                )
            ||]
    , tokA @"con_sym"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConSym n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a con_sym token."
            ||]
    ]

rVarIdExt :: RuleExpr (Ast.Name, Spanned.Span)
rVarIdExt = ruleExpr
    [ tokA @"_"
        <:> \(_ :* kwildcard :* HNil) ->
            [||
                ( Ast.primNameWildcard
                , AstParsed.sp do lexToken $$(kwildcard)
                )
            ||]
    , tokA @"var_id"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarId n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a var_id token."
            ||]
    ]

rVarSymExt :: RuleExpr (Ast.Name, Spanned.Span)
rVarSymExt = ruleExpr
    [ tokA @"var_sym"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarSym n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a var_sym token."
            ||]
    ]


rDeclCon :: RuleExpr (Ast.Name, Spanned.Span)
rDeclCon = ruleExpr
    [ tokVarA @"(" <^> tokA @"con_sym" <^> tokVarA @")"
        <:> \(_ :* kop :* _ :* t :* _ :* kcp :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConSym n ->
                    ( n
                    , AstParsed.sp ($$(kop), st, $$(kcp))
                    )
                _ ->
                    error "unreachable: expect a con_sym token."
            ||]
    , tokVarA @"(" <^> tokA @"con_id" <^> tokVarA @")"
        <:> \(_ :* kop :* _ :* t :* _ :* kcp :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConId n ->
                    ( n
                    , AstParsed.sp ($$(kop), st, $$(kcp))
                    )
                _ ->
                    error "unreachable: expect a con_id token."
            ||]
    , tokA @"con_id"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConId n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a con_id token."
            ||]
    ]

rDeclConOp :: RuleExpr (Ast.Name, Spanned.Span)
rDeclConOp = ruleExpr
    [ tokA @"`" <^> tokA @"con_sym" <^> tokA @"`"
        <:> \(_ :* ktick1 :* _ :* t :* _ :* ktick2 :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConSym n ->
                    ( n
                    , AstParsed.sp (lexToken $$(ktick1), st, lexToken $$(ktick2))
                    )
                _ ->
                    error "unreachable: expect a con_sym token."
            ||]
    , tokA @"`" <^> tokA @"con_id" <^> tokA @"`"
        <:> \(_ :* ktick1 :* _ :* t :* _ :* ktick2 :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConId n ->
                    ( n
                    , AstParsed.sp (lexToken $$(ktick1), st, lexToken $$(ktick2))
                    )
                _ ->
                    error "unreachable: expect a con_sym token."
            ||]
    , tokA @"con_sym"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdConSym n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a con_sym token."
            ||]
    ]

rDeclVar :: RuleExpr (Ast.Name, Spanned.Span)
rDeclVar = ruleExpr
    [ tokVarA @"(" <^> tokA @"var_sym" <^> tokVarA @")"
        <:> \(_ :* kop :* _ :* t :* _ :* kcp :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarSym n ->
                    ( n
                    , AstParsed.sp ($$(kop), st, $$(kcp))
                    )
                _ ->
                    error "unreachable: expect a var_sym token."
            ||]
    , tokVarA @"(" <^> tokA @"var_id" <^> tokVarA @")"
        <:> \(_ :* kop :* _ :* t :* _ :* kcp :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarId n ->
                    ( n
                    , AstParsed.sp ($$(kop), st, $$(kcp))
                    )
                _ ->
                    error "unreachable: expect a var_id token."
            ||]
    , tokA @"var_id"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarId n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a var_id token."
            ||]
    ]

rDeclOp :: RuleExpr (Ast.Name, Spanned.Span)
rDeclOp = ruleExpr
    [ tokA @"`" <^> tokA @"var_sym" <^> tokA @"`"
        <:> \(_ :* ktick1 :* _ :* t :* _ :* ktick2 :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarSym n ->
                    ( n
                    , AstParsed.sp (lexToken $$(ktick1), st, lexToken $$(ktick2))
                    )
                _ ->
                    error "unreachable: expect a var_sym token."
            ||]
    , tokA @"`" <^> tokA @"var_id" <^> tokA @"`"
        <:> \(_ :* ktick1 :* _ :* t :* _ :* ktick2 :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarId n ->
                    ( n
                    , AstParsed.sp (lexToken $$(ktick1), st, lexToken $$(ktick2))
                    )
                _ ->
                    error "unreachable: expect a var_id token."
            ||]
    , tokA @"var_sym"
        <:> \(_ :* t :* HNil) ->
            [||let st = lexToken $$(t) in case unSpanned st of
                Token.IdVarSym n ->
                    ( n
                    , AstParsed.sp st
                    )
                _ ->
                    error "unreachable: expect a var_sym token."
            ||]
    ]


rLsemis :: RuleExpr (Maybe Spanned.Span)
rLsemis = ruleExpr
    [ varA @"lsemi" <^> varA @"lsemis?"
        <:> \(lsemi :* mayLsemis :* HNil) ->
            [||AstParsed.maySp ($$(lsemi), $$(mayLsemis))||]
    ]

rMayLsemis :: RuleExpr (Maybe Spanned.Span)
rMayLsemis = ruleExpr
    [ varA @"lsemi" <^> varA @"lsemis?"
        <:> \(lsemi :* mayLsemis :* HNil) ->
            [||AstParsed.maySp ($$(lsemi), $$(mayLsemis))||]
    , eps
        <:> \HNil ->
            [||Nothing||]
    ]

rLsemi :: RuleExpr (Maybe Spanned.Span)
rLsemi = ruleExpr
    [ tokA @"<n>"
        <::> \(_ :* t :* HNil) ->
            [||case $$(t) of
                Layout.Newline newPos -> do
                    ctx <- Ptera.getAction
                    case layoutStack ctx of
                        ImplicitLayout curPos:_ | curPos == newPos ->
                            pure Nothing
                        ExplicitScopedLayout curPos:_ | curPos == newPos ->
                            pure Nothing
                        _ ->
                            Ptera.failAction
                _ ->
                    error "unreachable: expect a newline token."
            ||]
    , tokA @";"
        <:> \(_ :* t :* HNil) ->
            [||Just do AstParsed.sp do lexToken $$(t)||]
    ]


rImpBc :: RuleExpr ()
rImpBc = ruleExpr
    [ eps
        <::> \HNil ->
            [||popImplicitLayout||]
    ]

rSkip :: RuleExpr ()
rSkip = ruleExpr
    [ Ptera.tokA @"<n>"
        <::> \(t :* HNil) ->
            [||case $$(t) of
                Layout.Newline newPos -> do
                    ctx <- Ptera.getAction
                    case layoutStack ctx of
                        ImplicitLayout curPos:_ -> if
                            | curPos < newPos ->
                                pure ()
                            | otherwise ->
                                Ptera.failAction
                        ExplicitScopedLayout curPos:_ -> if
                            | curPos < newPos ->
                                pure ()
                            | otherwise ->
                                Ptera.failAction
                        NoLayout:_ ->
                            pure ()
                        [] ->
                            pure ()
                _ ->
                    error "unreachable: expect a newline token."
            ||]
    , eps
        <:> \HNil ->
            [||()||]
    ]


rLiteral :: RuleExpr (Ast.Lit AstParsed.T)
rLiteral = ruleExpr
    [ tokA @"bytechar"
        <:> \(_ :* bytechar :* HNil) ->
            [||let t = lexToken $$(bytechar) in
                case unSpanned t of
                    Token.LitByteChar v ->
                        Ast.LitByteChar v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a bytechar literal token."
            ||]
    , tokA @"bytestring"
        <:> \(_ :* bytestring :* HNil) ->
            [||let t = lexToken $$(bytestring) in
                case unSpanned t of
                    Token.LitByteString v ->
                        Ast.LitByteString v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a bytestring literal token."
            ||]
    , tokA @"integer"
        <:> \(_ :* integer :* HNil) ->
            [||let t = lexToken $$(integer) in
                case unSpanned t of
                    Token.LitInteger v ->
                        Ast.LitInteger v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a integer literal token."
            ||]
    , tokA @"rational"
        <:> \(_ :* rational :* HNil) ->
            [||let t = lexToken $$(rational) in
                case unSpanned t of
                    Token.LitRational v ->
                        Ast.LitRational v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a rational literal token."
            ||]
    , tokA @"char"
        <:> \(_ :* char :* HNil) ->
            [||let t = lexToken $$(char) in
                case unSpanned t of
                    Token.LitChar v ->
                        Ast.LitChar v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a char literal token."
            ||]
    , tokA @"string"
        <:> \(_ :* string :* HNil) ->
            [||let t = lexToken $$(string) in
                case unSpanned t of
                    Token.LitString v ->
                        Ast.LitString v do AstParsed.sp t
                    _ ->
                        error "unreachable: expect a string literal token."
            ||]
    ]

rMayTypeSig :: RuleExpr (Maybe (Ast.TypeExpr AstParsed.T), Maybe Spanned.Span)
rMayTypeSig = ruleExpr
    [ tokA @":" <^> varA @"type"
        <:> \(_ :* kcolon :* ty :* HNil) ->
            [||
                ( Just $$(ty)
                , Just do AstParsed.sp (lexToken $$(kcolon), $$(ty))
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( Nothing
                , Nothing
                )
            ||]
    ]


tokA :: forall t.
    Ptera.TokensMember Tokens t => TypedExpr '[(), Token]
tokA = varA @"skip" <^> Ptera.tokA @t

type family RuleSymbol (t :: Symbol) :: Symbol
type RuleSymbolReturnType t = Ptera.RuleExprReturnType RuleDefs (RuleSymbol t)

class
    ( KnownSymbol (RuleSymbol t)
    , Ptera.LiftType (RuleSymbolReturnType t)
    , Ptera.TokensMember Tokens t
    ) => RuleSymbolAction t where

    ruleSymbolActionM :: proxy t -> Token -> ActionTask (RuleSymbolReturnType t)

ruleSymbolRule :: forall t proxy.
    KnownSymbol t => RuleSymbolAction t => proxy t -> RuleExpr (RuleSymbolReturnType t)
ruleSymbolRule p = ruleExpr
    [ Ptera.tokA @t
        <::> \(tok :* HNil) ->
            [||ruleSymbolActionM $$(pt) $$(tok)||]
    ]
    where
        pt :: TH.Q (TH.TExp (Proxy t))
        pt = TH.unsafeTExpCoerce [|Proxy :: Proxy $(pure do TH.LitT do TH.StrTyLit do symbolVal p)|]

tokVarA :: forall t. RuleSymbolAction t => TypedExpr '[(), RuleSymbolReturnType t]
tokVarA = varA @"skip" <^> varA @(RuleSymbol t)

type instance RuleSymbol "(" = "tok_open_paren_without_skip"
instance RuleSymbolAction "(" where
    ruleSymbolActionM _ t = do
        pushLayoutItem NoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "{" = "tok_open_brace_without_skip"
instance RuleSymbolAction "{" where
    ruleSymbolActionM _ t = do
        pushLayoutItem NoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "[" = "tok_open_brack_without_skip"
instance RuleSymbolAction "[" where
    ruleSymbolActionM _ t = do
        pushLayoutItem NoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "interp_string_start" = "tok_interp_string_start_without_skip"
instance RuleSymbolAction "interp_string_start" where
    ruleSymbolActionM _ t = do
        pushLayoutItem NoLayout
        pure do interpStringLit do lexToken t

type instance RuleSymbol ")" = "tok_close_paren_without_skip"
instance RuleSymbolAction ")" where
    ruleSymbolActionM _ t = do
        popNoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "}" = "tok_close_brace_without_skip"
instance RuleSymbolAction "}" where
    ruleSymbolActionM _ t = do
        popNoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "]" = "tok_close_brack_without_skip"
instance RuleSymbolAction "]" where
    ruleSymbolActionM _ t = do
        popNoLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "interp_string_end" = "tok_interp_string_end_without_skip"
instance RuleSymbolAction "interp_string_end" where
    ruleSymbolActionM _ t = do
        popNoLayout
        pure do interpStringLit do lexToken t

type instance RuleSymbol "interp_string_cont" = "tok_interp_string_cont_without_skip"
instance RuleSymbolAction "interp_string_cont" where
    ruleSymbolActionM _ t = do
        popNoLayout
        pushLayoutItem NoLayout
        pure do interpStringLit do lexToken t

type instance RuleSymbol "{{" = "tok_open_dbrace_without_skip"
instance RuleSymbolAction "{{" where
    ruleSymbolActionM _ t = do
        let sp = AstParsed.sp do lexToken t
            pos = Layout.PositionByCol do
                Spanned.locCol do Spanned.beginLoc sp
        pushLayoutItem do ExplicitScopedLayout pos
        pure sp

type instance RuleSymbol "}}" = "tok_close_dbrace_without_skip"
instance RuleSymbolAction "}}" where
    ruleSymbolActionM _ t = do
        popExplicitScopedLayout
        pure do AstParsed.sp do lexToken t

type instance RuleSymbol "{n}" = "tok_new_implicit_layout_without_skip"
instance RuleSymbolAction "{n}" where
    ruleSymbolActionM _ = \case
        Layout.ExpectNewImplicitLayout expectPos -> do
            ctx <- Ptera.getAction
            let newPos = case layoutStack ctx of
                    ImplicitLayout curPos:_ -> if
                        | curPos < expectPos ->
                            expectPos
                        | otherwise ->
                            Layout.nextPosition expectPos
                    ExplicitScopedLayout curPos:_ -> if
                        | curPos < expectPos ->
                            expectPos
                        | otherwise ->
                            Layout.nextPosition expectPos
                    NoLayout:_ ->
                        expectPos
                    [] ->
                        expectPos
            pushLayoutItem
                do ImplicitLayout newPos
        _ ->
            error "unreachable: expect a new implicit layout open token."


pushLayoutItem :: LayoutItem -> ActionTask ()
pushLayoutItem item = Ptera.modifyAction \ctx -> ctx
    { gctxLayoutStack = item:layoutStack ctx
    }

popLayoutItem :: ActionTask ()
popLayoutItem = Ptera.modifyAction \ctx -> ctx
    { gctxLayoutStack = List.tail do layoutStack ctx
    }

popNoLayout :: ActionTask ()
popNoLayout = do
    layoutStack <$> Ptera.getAction >>= \case
        NoLayout:_ ->
            pure ()
        _ ->
            Ptera.failAction
    popLayoutItem

popExplicitScopedLayout :: ActionTask ()
popExplicitScopedLayout = do
    layoutStack <$> Ptera.getAction >>= \case
        ExplicitScopedLayout{}:_ ->
            pure ()
        _ ->
            Ptera.failAction
    popLayoutItem

popImplicitLayout :: ActionTask ()
popImplicitLayout = do
    layoutStack <$> Ptera.getAction >>= \case
        ImplicitLayout{}:_ ->
            pure ()
        _ ->
            Ptera.failAction
    popLayoutItem
