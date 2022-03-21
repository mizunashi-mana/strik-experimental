{-# LANGUAGE TemplateHaskell       #-}

module Language.Quell.Parsing.Parser.Rules where

import qualified Language.Haskell.TH              as TH
import           Language.Parser.Ptera.TH         (varA, eps, (<:>), (<::>))
import qualified Language.Parser.Ptera.TH         as Ptera
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Data.Bag as Bag


$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Layout.TokenWithL|]
    [ ("EOS",           [p|Layout.Token Token.EndOfSource|])

    , ("#as",           [p|Layout.Token Token.KwAs|])
    , ("#case",         [p|Layout.Token Token.KwCase|])
    , ("#data",         [p|Layout.Token Token.KwData|])
    , ("#default",      [p|Layout.Token Token.KwDefault|])
    , ("#derive",       [p|Layout.Token Token.KwDerive|])
    , ("#do",           [p|Layout.Token Token.KwDo|])
    , ("#export",       [p|Layout.Token Token.KwExport|])
    , ("#family",       [p|Layout.Token Token.KwFamily|])
    , ("#foreign",      [p|Layout.Token Token.KwForeign|])
    , ("#impl",         [p|Layout.Token Token.KwImpl|])
    , ("#in",           [p|Layout.Token Token.KwIn|])
    , ("#infix",        [p|Layout.Token Token.KwInfix|])
    , ("#let",          [p|Layout.Token Token.KwLet|])
    , ("#letrec",       [p|Layout.Token Token.KwLetrec|])
    , ("#match",        [p|Layout.Token Token.KwMatch|])
    , ("#module",       [p|Layout.Token Token.KwModule|])
    , ("#newtype",      [p|Layout.Token Token.KwNewtype|])
    , ("#pattern",      [p|Layout.Token Token.KwPattern|])
    , ("#rec",          [p|Layout.Token Token.KwRec|])
    , ("#record",       [p|Layout.Token Token.KwRecord|])
    , ("#role",         [p|Layout.Token Token.KwRole|])
    , ("#self",         [p|Layout.Token Token.KwSelf|])
    , ("#sig",          [p|Layout.Token Token.KwSignature|])
    , ("#static",       [p|Layout.Token Token.KwStatic|])
    , ("#trait",        [p|Layout.Token Token.KwTrait|])
    , ("#type",         [p|Layout.Token Token.KwType|])
    , ("#use",          [p|Layout.Token Token.KwUse|])
    , ("#with",         [p|Layout.Token Token.KwWith|])
    , ("#when",         [p|Layout.Token Token.KwWhen|])
    , ("#where",        [p|Layout.Token Token.KwWhere|])
    , ("#yield",        [p|Layout.Token Token.KwYield|])

    , ("#Default",      [p|Layout.Token Token.LKwDefault|])
    , ("#Self",         [p|Layout.Token Token.LKwSelf|])

    , ("->",            [p|Layout.Token Token.SymArrow|])
    , ("@",             [p|Layout.Token Token.SymAt|])
    , ("!",             [p|Layout.Token Token.SymBang|])
    , (":",             [p|Layout.Token Token.SymColon|])
    , ("=>",            [p|Layout.Token Token.SymDArrow|])
    , ("<=",            [p|Layout.Token Token.SymDLeftArrow|])
    , ("=",             [p|Layout.Token Token.SymEqual|])
    , ("^",             [p|Layout.Token Token.SymForall|])
    , ("\\",            [p|Layout.Token Token.SymLambda|])
    , ("<-",            [p|Layout.Token Token.SymLeftArrow|])
    , ("|",             [p|Layout.Token Token.SymOr|])
    , ("~",             [p|Layout.Token Token.SymTilde|])
    , ("_",             [p|Layout.Token Token.SymUnderscore|])
    , ("?",             [p|Layout.Token Token.SymUnknown|])

    , ("`",             [p|Layout.Token Token.SpBackquote|])
    , ("##",            [p|Layout.Token Token.SpBlock|])
    , ("[",             [p|Layout.Token Token.SpBrackOpen|])
    , ("]",             [p|Layout.Token Token.SpBrackClose|])
    , (",",             [p|Layout.Token Token.SpComma|])
    , ("{",             [p|Layout.Token Token.SpBraceOpen|])
    , ("}",             [p|Layout.Token Token.SpBraceClose|])
    , ("{{",            [p|Layout.Token Token.SpDBraceOpen|])
    , ("}}",            [p|Layout.Token Token.SpDBraceClose|])
    , (".",             [p|Layout.Token Token.SpDot|])
    , ("..",            [p|Layout.Token Token.SpDots|])
    , ("(",             [p|Layout.Token Token.SpParenOpen|])
    , (")",             [p|Layout.Token Token.SpParenClose|])
    , (";",             [p|Layout.Token Token.SpSemi|])
    , ("#>",            [p|Layout.Token Token.SpThen|])
    , ("#@",            [p|Layout.Token Token.SpTypeBlock|])
    , ("v{",            [p|Layout.Token Token.SpVBraceOpen|])
    , ("v}",            [p|Layout.Token Token.SpVBraceClose|])
    , ("v;",            [p|Layout.Token Token.SpVSemi|])

    , ("con_id",        [p|Layout.Token Token.IdConId{}|])
    , ("con_sym",       [p|Layout.Token Token.IdConSym{}|])
    , ("var_id",        [p|Layout.Token Token.IdVarId{}|])
    , ("var_sym",       [p|Layout.Token Token.IdVarSym{}|])

    , ("bytechar",      [p|Layout.Token Token.LitByteChar{}|])
    , ("bytestring",    [p|Layout.Token Token.LitByteString{}|])
    , ("integer",       [p|Layout.Token Token.LitInteger{}|])
    , ("rational",      [p|Layout.Token Token.LitRational{}|])
    , ("char",          [p|Layout.Token Token.LitChar{}|])
    , ("string",        [p|Layout.Token Token.LitString{}|])

    , ("interp_string_without_interp",
                        [p|Layout.Token Token.LitInterpStringWithoutInterp{}|])
    , ("interp_string_start",
                        [p|Layout.Token Token.LitInterpStringStart{}|])
    , ("interp_string_cont",
                        [p|Layout.Token Token.LitInterpStringCont{}|])
    , ("interp_string_end",
                        [p|Layout.Token Token.LitInterpStringEnd{}|])

    , ("{n}",           [p|Layout.ExpectNewImplicitLayout{}|])
    , ("<n>",           [p|Layout.Newline{}|])
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

$(Ptera.genRules
    do TH.mkName "RuleDefs"
    do Ptera.GenRulesTypes
        { Ptera.genRulesCtxTy = [t|GrammarContext|]
        , Ptera.genRulesTokensTy = [t|Tokens|]
        , Ptera.genRulesTokenTy = [t|Layout.TokenWithL|]
        }
    [ (TH.mkName "rdProgramEos", "program EOS", [t|Ast.Program AstParsed.T|])
    , (TH.mkName "rdProgram", "program", [t|Ast.Program AstParsed.T|])

    , (TH.mkName "rdDeclBody", "decl_body", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItems", "decl_items", [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItems0", "(decl_item lsemis)* decl_item?",
        [t|([Ast.Decl AstParsed.T], Maybe Spanned.Span)|])
    , (TH.mkName "rdDeclItem", "decl_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdTypeDecl", "type_decl", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdDataDecl", "data_decl", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdValDecl", "val_decl", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdSigItem", "sig_item", [t|Ast.Decl AstParsed.T|])

    , (TH.mkName "rdLsemis", "lsemis", [t|Maybe Spanned.Span|])
    , (TH.mkName "rdMayLsemis", "lsemis?", [t|Maybe Spanned.Span|])
    , (TH.mkName "rdLsemi", "lsemi", [t|Maybe Spanned.Span|])
    ])

$(Ptera.genParsePoints
    do TH.mkName "ParsePoints"
    do TH.mkName "RuleDefs"
    [ "program EOS"
    ])

grammar :: Ptera.GrammarM GrammarContext RuleDefs Tokens Layout.TokenWithL ParsePoints
grammar = Ptera.fixGrammar $ RuleDefs
    {}

type RuleExpr = Ptera.RuleExprM GrammarContext RuleDefs Tokens Layout.TokenWithL
type Alt = Ptera.AltM GrammarContext RuleDefs Tokens Layout.TokenWithL
type Expr = Ptera.Expr RuleDefs Tokens Layout.TokenWithL
type SemAct = Ptera.SemActM GrammarContext


rProgramEos :: RuleExpr (Ast.Program AstParsed.T)
rProgramEos = ruleExpr
    [ varA @"program" <^> tokA @"EOS"
        <:> \(program :* _ :* _ :* HNil) ->
            program
    ]

rProgram :: RuleExpr (Ast.Program AstParsed.T)
rProgram = ruleExpr
    [ varA @"decl_body"
        <:> \(declBody :* HNil) ->
            [||case $$(declBody) of
                (declItems, ms) -> Program declItems ms
            ||]
    ]


rDeclBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDeclBody = ruleExpr
    [ varA @"exp_dbo" <^> varA @"decl_items" <^> varA @"exp_dbc"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms, $$(expDbc))
                )
            }||]
    , varA @"exp_bo" <^> varA @"decl_items" <^> varA @"exp_bc"
        <:> \(expBo :* declItems :* expBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expBo), ms, $$(expBc))
                )
            }||]
    , varA @"imp_bo" <^> varA @"decl_items" <^> varA @"imp_bc"
        <:> \(impBo :* declItems :* impBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Spanned.maySp [$$(impBo), ms, $$(impBc)]
                )
            }||]
    ]

rDeclItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDeclItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(decl_item lsemis)* decl_item?"
        <:> \(mayLsemis :* declItems :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Spanned.maySp [$$(mayLsemis), ms)]
                )
            }||]
    ]

rDeclItems0 :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDeclItems0 = ruleExpr
    [ varA @"decl_item" <^> varA @"lsemis" <^> varA @"(decl_item lsemis)* decl_item?"
        <:> \(declItem :* lsemis :* declItems :* HNil) ->
            [||case ($$(declItem), $$(lsemis), $$(declItems)) of
                (declItem, ms1, (declItems, ms2)) ->
                    ( declItem:declItems
                    , Spanned.maySp (declItem, ms1, ms2)
                    )
            ||]
    , varA @"decl_item"
        <:> \(declItem :* HNil) ->
            [||([$$(declItem)], Just do Spanned.sp $$(declItem))||]
    , eps
        <:> \HNil ->
            [||([], Nothing)||]
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
        <:> \(ktype :* declcon :* kcolon :* ty :* HNil) ->
            [||
                Ast.DeclTypeSig $$(declcon) $$(ty) do
                    Spanned.sp ($$(ktype), $$(declcon), $$(kcolon), $$(ty))
            ||]
    ]

rValSigDecl :: RuleExpr (Ast.Decl AstParsed.T)
rValSigDecl = ruleExpr
    [ varA @"declvar" <^> tokA @":" <^> varA @"type"
        <:> \(declvar :* kcolon :* ty :* HNil) ->
            [||
                Ast.DeclValSig $$(declvar) $$(ty) do
                    Spanned.sp ($$(declvar), $$(kcolon), $$(ty))
            ||]
    ]

rConSigDecl :: RuleExpr (Ast.Decl AstParsed.T)
rConSigDecl = ruleExpr
    [ varA @"declcon" <^> tokA @":" <^> varA @"type"
        <:> \(declcon :* kcolon :* ty :* HNil) ->
            [||
                Ast.DeclConSig $$(declcon) $$(ty) do
                    Spanned.sp ($$(declcon), $$(kcolon), $$(ty))
            ||]
    ]


rTypeDecl :: RuleExpr (Ast.Decl AstParsed.T)
rTypeDecl = ruleExpr
    [ tokA @"#type" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(ktype :* decltype :* keq :* ty :* kwhere :* typeDeclWhereBody :* HNil) ->
            [||case $$(typeDeclWhereBody) of { (whereItems, ms) ->
                Ast.DeclType $$(decltype) $$(ty) $$(whereItems) do
                    Spanned.sp
                        ( $$(ktype), $$(decltype), $$(keq), $$(ty)
                        , $$(kwhere) AstParsed.:<< ms
                        )
            }||]
    , tokA @"#type" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type"
        <:> \(ktype :* decltype :* keq :* ty :* HNil) ->
            [||
                Ast.DeclType $$(decltype) $$(ty) [] do
                    Spanned.sp ($$(ktype), $$(decltype), $$(keq), $$(ty))
            ||]
    ]

rTypeDeclWhereBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rTypeDeclWhereBody = ruleExpr
    [ varA @"exp_dbo" <^> varA @"type_decl_where_items" <^> varA @"exp_dbc"
        <:> \(expDbo :* whereItems :* expDbc :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( whereItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"exp_bo" <^> varA @"type_decl_where_items" <^> varA @"exp_bc"
        <:> \(expBo :* whereItems :* expBc :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( whereItems
                , Just do Spanned.sp ($$(expBo), ms AstParsed.:>> $$(expBc))
                )
            }||]
    , varA @"imp_bo" <^> varA @"type_decl_where_items" <^> varA @"imp_bc"
        <:> \(impBo :* whereItems :* impBc :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( whereItems
                , Spanned.maySp ($$(impBo), ms, $$(impBc))
                )
            }||]
    ]

rTypeDeclWhereItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rTypeDeclWhereItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(type_decl_where_item lsemis)* type_decl_where_item?"
        <:> \(mayLsemis :* whereItems :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( otoList whereItems
                , Spanned.maySp ($$(mayLsemis), ms)
                )
            }||]
    ]

rTypeDeclWhereItems0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rTypeDeclWhereItems0 = ruleExpr
    [ varA @"type_decl_where_item" <^> varA @"lsemis" <^> varA @"(type_decl_where_item lsemis)* type_decl_where_item?"
        <:> \(whereItem :* lsemis :* whereItems :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( cons whereItem whereItems
                , Just do Spanned.maySp (Just $$(whereItem), $$(lsemis), ms)
                )
            }||]
    , varA @"type_decl_where_item"
        <:> \(whereItem :* HNil) ->
            [||
                ( pure $$(whereItem)
                , Just do Spanned.sp $$(whereItem)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( mempty
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
        <:> \(kdata :* decltype :* keq :* algDataType :* kwhere :* typeDeclWhereBody :* HNil) ->
            [||case $$(algDataType) of { (implTypes, msImplTypes) ->
                case $$(typeDeclWhereBody) of { (whereItems, msWhereItems) ->
                    Ast.DeclAlgDataType $$(decltype) implTypes whereItems do
                        Spanned.sp
                            ( $$(kdata), $$(decltype)
                            , $$(keq) AstParsed.:<< msImplTypes
                            , $$(kwhere) AstParsed.:<< msWhereItems
                            )
                }
            }||]
    , tokA @"#data" <^> varA @"decltype" <^> tokA @"=" <^> varA @"alg_data_type"
        <:> \(kdata :* decltype :* keq :* algDataType :* HNil) ->
            [||case $$(algDataType) of { (implTypes, msImplTypes) ->
                Ast.DeclAlgDataType $$(decltype) implTypes [] do
                    Spanned.sp
                        ( $$(kdata), $$(decltype)
                        , $$(keq) AstParsed.:<< msImplTypes
                        )
            }||]
    , tokA @"#data" <^> varA @"decltype" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(kdata :* decltype :* kwhere :* typeDeclWhereBody :* HNil) ->
            [||case $$(typeDeclWhereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclAlgDataType $$(decltype) [] whereItems do
                    Spanned.sp
                        ( $$(kdata), $$(decltype)
                        , $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , tokA @"#data" <^> varA @"decltype"
        <:> \(kdata :* decltype :* HNil) ->
            [||
                Ast.DeclAlgDataType $$(decltype) [] [] do
                    Spanned.sp
                        ( $$(kdata), $$(decltype)
                        )
            ||]
    , tokA @"#data" <^> varA @"declcon" <^> varA @"(':' type)?" <^> tokA @"#where" <^> varA @"data_decl_body"
        <:> \(kdata :* declcon :* mayTy :* kwhere :* dataDeclBody :* HNil) ->
            [||case $$(declcon) of { (declcon, spDeclcon) ->
                case $$(mayTy) of { (mayTy, msMayTy) ->
                    case $$(dataDeclBody) of { (dataDeclItems, msDataDeclItems) ->
                        Ast.DeclDataType declcon mayTy dataDeclItems do
                            Spanned.sp
                                ( $$(kdata), spDeclcon AstParsed.:<< msMayTy
                                , $$(kwhere) AstParsed.:<< msWhereItems
                                )
                    }
                }
            }||]
    , tokA @"#data" <^> varA @"declcon" <^> varA @"(':' type)?"
        <:> \(kdata :* declcon :* mayTy :* HNil) ->
            [||case $$(declcon) of { (declcon, spDeclcon) ->
                case $$(mayTy) of { (mayTy, msMayTy) ->
                    Ast.DeclDataType declcon mayTy dataDeclItems do
                        Spanned.sp
                            ( $$(kdata), spDeclcon AstParsed.:<< msMayTy
                            , $$(kwhere) AstParsed.:<< msWhereItems
                            )
                }
            }||]
    , tokA @"#newtype" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type" <^> tokA @"#where" <^> varA @"type_decl_where_body"
        <:> \(knewtype :* decltype :* keq :* ty :* kwhere :* typeDeclWhereBody :* HNil) ->
            [||case $$(typeDeclWhereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclNewType $$(decltype) $$(ty) whereItems do
                    Spanned.sp
                        ( $$(kdata), $$(decltype)
                        , $$(keq), $$(ty)
                        , $$(kwhere) AstParsed.:<< msWhereItems
                        )
            }||]
    , tokA @"#newtype" <^> varA @"decltype" <^> tokA @"=" <^> varA @"type"
        <:> \(knewtype :* decltype :* keq :* ty :* HNil) ->
            [||
                Ast.DeclNewType $$(decltype) $$(ty) [] do
                    Spanned.sp
                        ( $$(kdata), $$(decltype)
                        , $$(keq), $$(ty)
                        )
            ||]
    ]

rDataDeclBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDataDeclBody = ruleExpr
    [ varA @"exp_dbo" <^> varA @"data_decl_items" <^> varA @"exp_dbc"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"exp_bo" <^> varA @"data_decl_items" <^> varA @"exp_bc"
        <:> \(expBo :* declItems :* expBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expBo), ms AstParsed.:>> $$(expBc))
                )
            }||]
    , varA @"imp_bo" <^> varA @"data_decl_items" <^> varA @"imp_bc"
        <:> \(impBo :* declItems :* impBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Spanned.maySp ($$(impBo), ms, $$(impBc))
                )
            }||]
    ]

rDataDeclItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rDataDeclItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(data_decl_item lsemis)* data_decl_item?"
        <:> \(mayLsemis :* declItems :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( otoList declItems
                , Spanned.maySp ($$(mayLsemis), ms)
                )
            }||]
    ]

rDataDeclItems0 :: RuleExpr (Bag.T (Ast.Decl AstParsed.T), Maybe Spanned.Span)
rDataDeclItems0 = ruleExpr
    [ varA @"data_decl_item" <^> varA @"lsemis" <^> varA @"(data_decl_item lsemis)* data_decl_item?"
        <:> \(dataDeclItem :* lsemis :* declItems :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( cons $$(dataDeclItem) declItems
                , Spanned.maySp (Just $$(dataDeclItem), $$(lsemis), ms)
                )
            }||]
    , varA @"data_decl_item"
        <:> \(dataDeclItem :* HNil) ->
            [||
                ( pure $$(dataDeclItem)
                , Just do Spanned.sp $$(dataDeclItem)
                )
            }||]
    , eps
        <:> \HNil ->
            [||
                ( mempty
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

rAlgDataType :: RuleExpr ([Ast.ImplType AstParsed.T], Maybe Spanned.Span)
rAlgDataType = ruleExpr
    [ varA @"alg_data_type_items"
        <:> \(algDataTypeItems :* HNil) ->
            [||case $$(algDataTypeItems) of { (implTypes, ms) ->
                ( otoList implTypes
                , ms
                )
            }||]
    ]

rAlgDataTypeItems :: RuleExpr (Bag.T (Ast.ImplType AstParsed.T), Maybe Spanned.Span)
rAlgDataTypeItems = ruleExpr
    [ tokA @"|" <^> varA @"(impltype '|')* impltype?"
        <:> \(kmid :* implTypes :* HNil) ->
            [||case $$(implTypes) of { (implTypes, ms) ->
                ( implTypes
                , Just do Spanned.sp do $$(kmid) AstParsed.:<< ms
                )
            }||]
    , varA @"(impltype '|')* impltype?"
        <:> \(implTypes :* HNil) ->
            implTypes
    ]

rAlgDataTypeItems0 :: RuleExpr (Bag.T (Ast.ImplType AstParsed.T), Maybe Spanned.Span)
rAlgDataTypeItems0 = ruleExpr
    [ varA @"impltype" <^> tokA @"|" <^> varA @"(impltype '|')* impltype?"
        <:> \(implType :* kmid :* implTypes :* HNil) ->
            [||case $$(implTypes) of { (implTypes, ms) ->
                ( cons $$(implType) implTypes
                , Just do Spanned.sp ($$(implType), $$(kmid) AstParsed.:<< ms)
                )
            }||]
    , varA @"impltype"
        <:> \(implType :* HNil) ->
            [||
                ( pure $$(implType)
                , Just do Spanned.sp $$(implType)
                )
            ||]
    ]


rValDecl :: RuleExpr (Ast.Decl AstParsed.T)
rValDecl = ruleExpr
    [ varA @"declvarexpr" <^> varA @"(':' type)?" <^> tokA @"=" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(declVarExpr :* mayTy :* keq :* expr :* kwhere :* valDeclWhereBody :* HNil) ->
            [||case $$(mayTy) of { (mayTy, msMayTy) ->
                case $$(valDeclWhereBody) of { (whereItems, msWhereItems) ->
                    Ast.DeclVal $$(declVarExpr) mayTy $$(expr) whereItems do
                        Spanned.sp
                            ( $$(declVarExpr) AstParsed.:<< msMayTy
                            , $$(keq)
                            , $$(expr) AstParsed.:<< msWhereItems
                            )
                }
            }||]
    , varA @"declvarexpr" <^> varA @"(':' type)?" <^> tokA @"=" <^> varA @"expr"
        <:> \(declVarExpr :* mayTy :* keq :* expr :* HNil) ->
            [||case $$(mayTy) of { (mayTy, msMayTy) ->
                Ast.DeclVal $$(declVarExpr) mayTy $$(expr) [] do
                    Spanned.sp
                        ( $$(declVarExpr) AstParsed.:<< msMayTy
                        , $$(keq)
                        , $$(expr)
                        )
            }||]
    ]

rValBind :: RuleExpr (Ast.Decl AstParsed.T)
rValBind = ruleExpr
    [ varA @"pat" <^> tokA @"=" <^> varA @"expr" <^> tokA @"#where" <^> varA @"val_decl_where_body"
        <:> \(pat :* keq :* expr :* kwhere :* valDeclWhereBody :* HNil) ->
            [||case $$(valDeclWhereBody) of { (whereItems, msWhereItems) ->
                Ast.DeclValBind $$(pat) $$(expr) whereItems do
                    Spanned.sp ($$(pat), $$(keq), $$(expr) AstParsed.:<< msWhereItems)
            }||]
    , varA @"pat" <^> tokA @"=" <^> varA @"expr"
        <:> \(pat :* keq :* expr :* HNil) ->
            [||
                Ast.DeclValBind $$(pat) $$(expr) [] do
                    Spanned.sp ($$(pat), $$(keq), $$(expr))
            ||]
    ]

rValDeclWhereBody :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rValDeclWhereBody = ruleExpr
    [ varA @"exp_dbo" <^> varA @"val_decl_where_items" <^> varA @"exp_dbc"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"exp_bo" <^> varA @"val_decl_where_items" <^> varA @"exp_bc"
        <:> \(expBo :* declItems :* expBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expBo), ms AstParsed.:>> $$(expBc))
                )
            }||]
    , varA @"imp_bo" <^> varA @"val_decl_where_items" <^> varA @"imp_bc"
        <:> \(impBo :* declItems :* impBc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Spanned.maySp ($$(impBo), ms, $$(impBc))
                )
            }||]
    ]

rValDeclWhereItems :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rValDeclWhereItems = ruleExpr
    [ varA @"lsemis?" <^> varA @"(val_decl_where_item lsemis)* val_decl_where_item?"
        <:> \(mayLsemis :* whereItems :* HNil) ->
            [||case $$(whereItems) of { (declItems, ms) ->
                ( declItems
                , Spanned.maySp ($$(mayLsemis), ms)
                )
            }||]
    ]

rValDeclWhereItems0 :: RuleExpr ([Ast.Decl AstParsed.T], Maybe Spanned.Span)
rValDeclWhereItems0 = ruleExpr
    [ varA @"val_decl_where_item" <^> varA @"lsemis" <^> varA @"(val_decl_where_item lsemis)* val_decl_where_item?"
        <:> \(whereItem :* lsemis :* whereItems :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( cons $$(whereItem) whereItems
                , Spanned.maySp (Just $$(whereItem), $$(lsemis), ms)
                )
            }||]
    , varA @"val_decl_where_item"
        <:> \(whereItem :* HNil) ->
            [||
                ( pure $$(whereItem)
                , Just do Spanned.sp $$(whereItem)
                )
            ||]
    , eps
        <:> \HNil ->
            [||
                ( mempty
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
    [ varA @"simple_bind_var" <^> varA @"declconop" <^> varA @"simple_bind_var"
        <:> \(bindVar1 :* declconop :* bindVar2 :* HNil) ->
            [||case $$(declconop) of { (declconop, spDeclconop) ->
                Ast.DeclInfixType $$(bindVar1) declconop $$(bindVar2) do
                    Spanned.sp ($$(bindVar1), spDeclconop, $$(bindVar2))
            }||]
    , varA @"declcon" <^> varA @"bind_var*"
        <:> \(declcon :* bindVars :* HNil) ->
            [||case $$(declcon) of { (declcon, spDeclcon) ->
                case $$(bindVars) of { (bindVars, ms) ->
                    Ast.DeclAppType declcon $$(bindVars) do
                        Spanned.sp do spDeclcon AstParsed.:<< ms
                }
            }||]
    ]

rImplType :: RuleExpr (Ast.ImplType AstParsed.T)
rImplType = ruleExpr
    [ varA @"type_qualified" <^> varA @"conop_qualified" <^> varA @"type_qualified"
        <:> \(ty1 :* conop :* ty2 :* HNil) ->
            [||case $$(conop) of { (conop, spConop) ->
                Ast.ImplInfixType $$(ty1) conop $$(ty2) do
                    Spanned.sp ($$(ty1), spConop, $$(ty2))
            }||]
    , varA @"con_qualified" <^> varA @"type_qualified*"
        <:> \(con :* types :* HNil) ->
            [||case $$(con) of { (con, spCon) ->
                case $$(types) of { (types, msTypes) ->
                    Ast.ImplAppType con types do
                        Spanned.sp do spCon AstParsed.:<< msTypes
                }
            }||]
    ]

rDeclVarExpr :: RuleExpr (Ast.DeclExpr AstParsed.T)
rDeclVarExpr = ruleExpr
    [ varA @"simple_bind_var" <^> varA @"declop" <^> varA @"simple_bind_var"
        <:> \(bindVar1 :* declop :* bindVar2 :* HNil) ->
            [||case $$(declop) of { (declop, spDeclop) ->
                Ast.DeclInfixExpr $$(bindVar1) declop $$(bindVar2) do
                    Spanned.sp ($$(bindVar1), spDeclop, $$(bindVar2))
            }||]
    , varA @"declvar" <^> varA @"bind_var*"
        <:> \(declvar :* bindVars :* HNil) ->
            [||case $$(declvar) of { (declVar, spDeclVar) ->
                case $$(bindVars) of { (bindVars, msBindVars) ->
                    Ast.DeclAppExpr declVar bindVars do
                        Spanned.sp do spDeclVar AstParsed.:<< msBindVars
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
            [||
                Ast.TypeInfix $$(typeApps) $$(typeOp) $$(typeInfix) do
                    Spanned.sp ($$(typeApps), $$(typeOp), $$(typeInfix))
            ||]
    , varA @"type_apps"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeOp :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeOp = ruleExpr
    [ tokA @"`" <^> varA @"type_qualified_op" <^> tokA @"`"
        <:> \(kbacktick1 :* tyOp :* kbacktick2 :* HNil) ->
            [||
                Ast.TypeAnn $$(tyOp) do
                    Spanned.sp ($$(kbacktick1), $$(tyOp), $$(kbacktick2))
            ||]
    , varA @"con_sym_ext"
        <:> \(conSymExt :* HNil) ->
            [||case $$(conSymExt) of { (conSymExt, spConSymExt) ->
                Ast.TypeCon conSymExt spConSymExt
            }||]
    , varA @"var_sym_ext"
        <:> \(varSymExt :* HNil) ->
            [||case $$(varSymExt) of { (varSymExt, spVarSymExt) ->
                Ast.TypeVar varSymExt spVarSymExt
            }||]
    ]

rTypeQualifiedOp :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeQualifiedOp = ruleExpr
    [ varA @"con_sym_ext"
        <:> \(conSymExt :* HNil) ->
            [||case $$(conSymExt) of { (conSymExt, spConSymExt) ->
                Ast.TypeCon conSymExt spConSymExt
            }||]
    , varA @"var_sym_ext"
        <:> \(varSymExt :* HNil) ->
            [||case $$(varSymExt) of { (varSymExt, spVarSymExt) ->
                Ast.TypeVar varSymExt spVarSymExt
            }||]
    , varA @"type_apps"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeApps :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeApps = ruleExpr
    [ varA @"type_qualified" <^> varA @"type_app+"
        <:> \(ty :* types :* HNil) ->
            [||case $$(types) of { (types, spTypes) ->
                Ast.TypeApp $$(ty)
                    do otoList types
                    do Spanned.sp ($$(ty), spTypes)
            }||]
    , varA @"type_qualified"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeApps1 :: RuleExpr (Bag.T (Ast.TypeExpr AstParsed.T), Spanned.Span)
rTypeApps1 = ruleExpr
    [ varA @"type_app" <^> varA @"type_app+"
        <:> \(ty :* types :* HNil) ->
            [||case $$(types) of { (types, spTypes) ->
                ( cons $$(ty) types
                , Spanned.sp ($$(ty), spTypes)
                )
            }||]
    , varA @"type_app"
        <:> \(ty :* HNil) ->
            [||
                ( pure $$(ty)
                , Spanned.sp $$(ty)
                )
            ||]
    ]

rTypeApp :: RuleExpr (Ast.TypeExpr AstParse.T, Spanned.Span)
rTypeApp = ruleExpr
    [ tokA @"@" <^> varA @"type_qualified"
        <:> \(kat :* ty :* HNil) ->
            [||
                ( $$(ty)
                , Spanned.sp ($$(kat), $$(ty))
                )
            ||]
    , tokA @"#@" <^> varA @"type_block_body"
        <:> \(kbat :* ty :* HNil) ->
            [||
                ( $$(ty)
                , Spanned.sp ($$(kbat), $$(ty))
                )
            ||]
    , varA @"type_qualified"
        <:> \(ty :* HNil) ->
            ty
    ]




rSigItem :: RuleExpr (Ast.Decl AstParsed.T)
rSigItem = undefined


rLsemis :: RuleExpr (Maybe Spanned.Span)
rLsemis = ruleExpr
    [ varA @"lsemi" <^> varA @"lsemis?"
        <:> \(lsemi :* mayLsemis :* HNil) ->
            [||Spanned.maySp (lsemi, mayLsemis)||]
    ]

rMayLsemis :: RuleExpr (Maybe Spanned.Span)
rMayLsemis = ruleExpr
    [ varA @"lsemi" <^> varA @"lsemis?"
        <:> \(lsemi :* mayLsemis :* HNil) ->
            [||Spanned.maySp (lsemi, mayLsemis)||]
    , eps
        <:> \HNil ->
            [||Nothing||]
    ]

rLsemi :: RuleExpr (Maybe Spanned.Span)
rLsemi = ruleExpr
    [ Ptera.tokA @"<n>"
        <::> \(t :* HNil) ->
            [||case $$(t) of { Layout.Newline lpos1 -> do
                ctx <- Ptera.getAction
                case layoutStack ctx of
                    ImplicitLayout lpos2:_ | lpos1 == lpos2 ->
                        pure Nothing
                    ExplicitScopedLayout lpos2:_ | lpos1 == lpos2 ->
                        pure Nothing
                    NoLayout:_ ->
                        Ptera.failAction
                    [] ->
                        Ptera.failAction
            }||]
    , tokA @";"
        <:> \(_ :* t :* HNil) ->
            [||case $$(t) of { Layout.Token t ->
                Just do Spanned.getSpan t
            }||]
    ]
