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
    [ varA @"{{" <^> varA @"decl_items" <^> varA @"}}"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms, $$(expDbc))
                )
            }||]
    , varA @"{" <^> varA @"decl_items" <^> varA @"}"
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
    [ varA @"{{" <^> varA @"type_decl_where_items" <^> varA @"}}"
        <:> \(expDbo :* whereItems :* expDbc :* HNil) ->
            [||case $$(whereItems) of { (whereItems, ms) ->
                ( whereItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"{" <^> varA @"type_decl_where_items" <^> varA @"}"
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
    [ varA @"{{" <^> varA @"data_decl_items" <^> varA @"}}"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"{" <^> varA @"data_decl_items" <^> varA @"}"
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
    [ varA @"{{" <^> varA @"val_decl_where_items" <^> varA @"}}"
        <:> \(expDbo :* declItems :* expDbc :* HNil) ->
            [||case $$(declItems) of { (declItems, ms) ->
                ( declItems
                , Just do Spanned.sp ($$(expDbo), ms AstParsed.:>> $$(expDbc))
                )
            }||]
    , varA @"{" <^> varA @"val_decl_where_items" <^> varA @"}"
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
            [||
                ( $$(ty)
                , Spanned.sp $$(ty)
                )
            ||]
    ]

rTypeQualified :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeQualified = ruleExpr
    [ varA @"type_block"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeBlock :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeBlock = ruleExpr
    [ tokA @"^" <^> varA @"bind_var*" <^> tokA @"#>" <^> varA @"type"
        <:> \(kcaret :* bindVars :* karr :* ty :* HNil) ->
            [||Ast.TypeForall $$(bindVars) $$(ty) do
                Spanned.sp
                    ( $$(kcaret) AstParsed.:<< $$(bindVars)
                    , $$(karr), $$(ty)
                    )
            ||]
    , tokA @"##" <^> varA @"type_block_body"
        <:> \(kblock :* ty :* HNil) ->
            [||Ast.TypeAnn $$(ty) do
                Spanned.sp ($$(kblock), $$(ty))
            ||]
    , varA @"type_atomic"
        <:> \(ty :* HNil) ->
            ty
    ]

rTypeAtomic :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeAtomic = ruleExpr
    [ varA @"(" <^> varA @"type" <^> tokA @":" <^> varA @"type" <^> varA @")"
        <:> \(kparenl :* ty1 :* kcolon :* ty2 :* kparenr :* HNil) ->
            [||Ast.TypeSig $$(ty1) $$(ty2) do
                Spanned.sp ($$(kparenl), $$(ty1), $$(kcolon), $$(ty2), $$(kparenr))
            ||]
    , varA @"(" <^> varA @"type" <^> varA @")"
        <:> \(kparenl :* ty :* kparenr :* HNil) ->
            [||Ast.TypeAnn $$(ty) do
                Spanned.sp ($$(kparenl), $$(ty), $$(kparenr))
            ||]
    , varA @"type_literal"
        <:> \(ty :* HNil) ->
            ty
    , varA @"con"
        <:> \(con :* HNil) ->
            [||case $$(con) of { (con, spCon) ->
                Ast.TypeCon con spCon
            }||]
    , varA @"var"
        <:> \(var :* HNil) ->
            [||case $$(var) of { (var, spVar) ->
                Ast.TypeVar var spVar
            }||]
    ]

rTypeLiteral :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeLiteral = ruleExpr
    [ varA @"literal"
        <:> \(lit :* HNil) ->
            [||Ast.TypeLit $$(lit) do
                Spanned.sp $$(lit)
            ||]
    , varA @"(" <^> varA @"type_tuple_items" <^> varA @")"
        <:> \(kparenl :* typeTupleItems :* kparenr :* HNil) ->
            [||case $$(typeTupleItems) of { (items, spItems) ->
                Ast.TypeTuple items do
                    Spanned.sp ($$(kparenl), spItems, $$(kparenr))
            }||]
    , varA @"[" <^> varA @"type_array_items" <^> varA @"]"
        <:> \(kbrackl :* typeArrayItems :* kbrackr :* HNil) ->
            [||case $$(typeArrayItems) of { (items, msItems) ->
                Ast.TypeArray items do
                    Spanned.sp ($$(kbrackl), msItems AstParsed.:>> $$(kbrackr))
            }||]
    , varA @"{" <^> varA @"type_simplrecord_items" <^> varA @"}"
        <:> \(kbracel :* typeRecordItems :* kbracer :* HNil) ->
            [||case $$(typeRecordItems) of { (items, msItems) ->
                Ast.TypeRecord items do
                    Spanned.sp ($$(kbracel), msItems AstParsed.:>> $$(kbracer))
            }||]
    ]

rTypeBlockBody :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeBlockBody = ruleExpr
    [ varA @"{{" <^> varA @"type_block_item" <^> varA @"}}"
        <:> \(expDbo :* typeBlockItem :* expDbc :* HNil) ->
            [||Ast.TypeAnn $$(typeBlockItem) do
                Spanned.sp ($$(expDbo), $$(typeBlockItem), $$(expDbc))
            ||]
    , varA @"{" <^> varA @"type_block_item" <^> varA @"}"
        <:> \(expBo :* typeBlockItem :* expBc :* HNil) ->
            [||Ast.TypeAnn $$(typeBlockItem) do
                Spanned.sp ($$(expBo), $$(typeBlockItem), $$(expBc))
            ||]
    , varA @"imp_bo" <^> varA @"type_block_item" <^> varA @"imp_bc"
        <:> \(impBo :* typeBlockItem :* impBc :* HNil) ->
            [||Ast.TypeAnn $$(typeBlockItem) do
                Spanned.sp ($$(impBo), $$(typeBlockItem), $$(impBc))
            ||]
    ]

rTypeBlockItem :: RuleExpr (Ast.TypeExpr AstParsed.T)
rTypeBlockItem = ruleExpr
    [ varA @"lsemis?" <^> varA @"type" <^> varA @"lsemis?"
        <:> \(mlsemis1 :* ty :* mlsemis2 :* HNil) ->
            [||TypeAnn $$(ty) do
                Spanned.sp
                    ($$(mlsemis1) AstParsed.:>> $$(ty) AstParsed.:<< $$(mlsemis2))
            ||]
    ]

rTypeTupleItems :: RuleExpr ([Ast.TypeExpr AstParsed.T], Spanned.Span)
rTypeTupleItems = ruleExpr
    [ tokA @"," <^> varA @"(type ',')+ type ','?"
        <:> \(kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, spItems) ->
                ( items
                , Spanned.sp ($$(kcomma), spItems)
                )
            }||]
    , varA @"(type ',')+ type ','?"
        <:> \(items :* HNil) ->
            items
    ]

rTypeTupleItems0 :: RuleExpr ([Ast.TypeExpr AstParsed.T], Spanned.Span)
rTypeTupleItems0 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')+ type ','?"
        <:> \(ty :* kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, spItems) ->
                ( cons $$(ty) items
                , Spanned.sp ($$(ty), $$(kcomma), spItems)
                )
            }||]
    , varA @"type" <^> tokA @"," <^> varA @"type" <^> tokA @","
        <:> \(ty1 :* kcomma1 :* ty2 :* kcomma2 :* HNil) ->
            [||
                ( cons $$(ty1) do pure $$(ty2)
                , Spanned.sp ($$(ty1), $$(kcomma1), $$(ty2), $$(kcomma2))
                )
            ||]
    , varA @"type" <^> tokA @"," <^> varA @"type"
        <:> \(ty1 :* kcomma1 :* ty2 :* HNil) ->
            [||
                ( cons $$(ty1) do pure $$(ty2)
                , Spanned.sp ($$(ty1), $$(kcomma1), $$(ty2))
                )
            ||]
    ]

rTypeArrayItems :: RuleExpr ([Ast.TypeExpr AstParsed.T], Maybe Spanned.Span)
rTypeArrayItems = ruleExpr
    [ tokA @"," <^> varA @"(type ',')* type?"
        <:> \(kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                ( items
                , Just do Spanned.sp do $$(kcomma) AstParsed.:>> msItems
                )
            }||]
    , varA @"(type ',')* type?"
        <:> \(items :* HNil) ->
            items
    ]

rTypeArrayItems0 :: RuleExpr ([Ast.TypeExpr AstParsed.T], Maybe Spanned.Span)
rTypeArrayItems0 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')* type?"
        <:> \(ty :* kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                ( cons $$(ty) items
                , Just do Spanned.sp ($$(ty), $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"type"
        <:> \(ty :* HNil) ->
            [||
                ( pure $$(ty)
                , Just do Spanned.sp $$(ty)
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

rTypeSimpleRecordItems :: RuleExpr ([Ast.TypeRecordItem AstParsed.T], Maybe Spanned.Span)
rTypeSimpleRecordItems = ruleExpr
    [ tokA @"," <^> varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                ( items
                , Just do Spanned.sp do $$(kcomma) AstParsed.:<< msItems
                )
            }||]
    , varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(items :* HNil) ->
            items
    ]

rTypeSimpleRecordItems0 :: RuleExpr ([Ast.TypeRecordItem AstParsed.T], Maybe Spanned.Span)
rTypeSimpleRecordItems0 = ruleExpr
    [ varA @"type_simplrecord_item" <^> tokA @"," <^> varA @"(type_simplrecord_item ',')* type_simplrecord_item?"
        <:> \(recordItem :* kcomma :* items :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                ( cons $$(recordItem) items
                , Just do Spanned.sp ($$(recordItem), $$(kcomma) AstParsed.:<< msItems)
                )
            }||]
    , varA @"type_simplrecord_item"
        <:> \(item :* HNil) ->
            [||
                ( pure $$(item)
                , Just do Spanned.sp item
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

rTypeSimpleRecordItem :: RuleExpr (Ast.TypeRecordItem AstParsed.T)
rTypeSimpleRecordItem = ruleExpr
    [ varA @"declvar" <^> tokA @":" <^> varA @"type"
        <:> \(declvar :* kcolon :* ty :* HNil) ->
            [||case $$(declvar) of { (declvar, spDeclVar) ->
                Ast.TypeRecordItem declvar $$(ty) do
                    Spanned.sp (spDeclVar, $$(kcolon), $$(ty))
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
        <:> \(expr :* kcolon :* ty :* HNil) ->
            [||Ast.ExprSig $$(expr) $$(ty) do
                Spanned.sp ($$(expr), $$(kcolon), $$(ty))
            ||]
    , varA @"expr_infix"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprInfix :: RuleExpr (Ast.Expr AstParsed.T)
rExprInfix = ruleExpr
    [ varA @"expr_apps" <^> varA @"expr_op" <^> varA @"expr_infix"
        <:> \(expr1 :* op :* expr2 :* HNil) ->
            [||Ast.ExprInfix $$(expr1) $$(op) $$(expr2) do
                Spanned.sp ($$(expr1), $$(op), $$(expr2))
            ||]
    , varA @"expr_apps"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprOp :: RuleExpr (Ast.Expr AstParsed.T)
rExprOp = ruleExpr
    [ tokA @"`" <^> varA @"expr_op_block" <^> tokA @"`"
        <:> \(kbacktick1 :* expr :* kbacktick2 :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(kbacktick1), $$(expr), $$(kbacktick2))
            ||]
    , varA @"con_sym_ext"
        <:> \(con :* HNil) ->
            [||case $$(con) of { (con, spCon) ->
                Ast.ExprCon con spCon
            }||]
    , varA @"var_sym_ext"
        <:> \(var :* HNil) ->
            [||case $$(var) of { (var, spVar) ->
                Ast.ExprVar var spVar
            }||]
    ]

rExprOpBlock :: RuleExpr (Ast.Expr AstParsed.T)
rExprOpBlock = ruleExpr
    [ varA @"con_sym_ext"
        <:> \(con :* HNil) ->
            [||case $$(con) of { (con, spCon) ->
                Ast.ExprCon con spCon
            }||]
    , varA @"var_sym_ext"
        <:> \(var :* HNil) ->
            [||case $$(var) of { (var, spVar) ->
                Ast.ExprVar var spVar
            }||]
    , varA @"expr_apps"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprApps :: RuleExpr (Ast.Expr AstParsed.T)
rExprApps = ruleExpr
    [ varA @"expr_qualified" <^> varA @"expr_app+"
        <:> \(expr :* exprs :* HNil) ->
            [||case $$(exprs) of { (exprs, spExprs) ->
                Ast.ExprApp $$(expr)
                    do otoList exprs
                    do Spanned.sp ($$(expr), spExprs)
            }||]
    , varA @"expr_qualified"
        <:> \(expr :* HNil) ->
            expr
    ]

rExprApps1 :: RuleExpr (Bag.T (Ast.Expr AstParsed.T), Spanned.Span)
rExprApps1 = ruleExpr
    [ varA @"expr_app" <^> varA @"expr_app+"
        <:> \(expr :* exprs :* HNil) ->
            [||case $$(exprs) of { (exprs, spExprs) ->
                ( cons $$(expr) exprs
                , Spanned.sp ($$(expr), spExprs)
                )
            }||]
    , varA @"expr_app"
        <:> \(expr :* HNil) ->
            [||
                ( pure $$(expr)
                , Spanned.sp $$(expr)
                )
            ||]
    ]

rExprApp :: RuleExpr (Ast.AppExpr AstParsed.T)
rExprApp = ruleExpr
    [ tokA @"@" <^> varA @"type_qualified"
        <:> \(kat :* ty :* HNil) ->
            [||Ast.UnivAppExpr $$(ty) do
                Spanned.sp ($$(kat), $$(ty))
            ||]
    , tokA @"#@" <^> varA @"type_block_body"
        <:> \(kat :* ty :* HNil) ->
            [||Ast.UnivAppExpr $$(ty) do
                Spanned.sp ($$(kat), $$(ty))
            ||]
    , varA @"expr_qualified"
        <:> \(expr :* HNil) ->
            [||Ast.AppExpr $$(expr) do
                Spanned.sp $$(expr)
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
        <:> \(kbackslash :* pats :* alts :* HNil) ->
            [||case $$(pats) of { (pats, msPats) ->
                case $$(alts) of { (alts, msAlts) ->
                    let msAlt = Spanned.maySp (msPats, msAlts)
                    in Ast.ExprLambda
                        [ Ast.CaseAlt
                            do otoList pats
                            alts msAlt
                        ]
                        do Spanned.sp
                            do $$(kbackslash) AstParsed.:<< msAlt
                }
            }||]
    , tokA @"#case" <^> varA @"case_alt_body"
        <:> \(kcase :* alts :* HNil) ->
            [||case $$(alts) of { (alts, msAlts) ->
                Ast.ExprLambda alts do
                    Spanned.sp do $$(kcase) AstParsed.:<< msAlts
            }||]
    , tokA @"#letrec" <^> varA @"let_binds" <^> tokA @"#in" <^> varA @"expr"
        <:> \(kletrec :* binds :* kin :* expr :* HNil) ->
            [||case $$(binds) of { (binds, msBinds) ->
                Ast.ExprLetrec binds $$(expr) do
                    Spanned.sp
                        ( $$(kletrec) AstParsed.:<< msBinds
                        , $$(kin), $$(expr)
                        )
            }||]
    , tokA @"#let" <^> varA @"let_binds" <^> tokA @"#in" <^> varA @"expr"
        <:> \(klet :* binds :* kin :* expr :* HNil) ->
            [||case $$(binds) of { (binds, msBinds) ->
                Ast.ExprLetrec binds $$(expr) do
                    Spanned.sp
                        ( $$(klet) AstParsed.:<< msBinds
                        , $$(kin), $$(expr)
                        )
            }||]
    , tokA @"#match" <^> varA @"','? (expr ',')* expr?" <^> tokA @"#with" <^> varA @"case_alt_body"
        <:> \(kmatch :* exprs :* kwith :* alts :* HNil) ->
            [||case $$(exprs) of { (exprs, msExprs) ->
                case $$(alts) of { (alts, msAlts) ->
                    Ast.ExprMatch exprs alts do
                        Spanned.sp
                            ( $$(kmatch) AstParsed.:<< msExprs
                            , $$(kwith) AstParsed.:<< msAlts
                            )
                }
            }||]
    , tokA @"#do" <^> varA @"do_body"
        <:> \(kdo :* doBody :* HNil) ->
            [||case $$(doBody) of { (stmts, expr, spBody) ->
                Ast.ExprDo stmts expr do
                    Spanned.sp ($$(kdo), spBody)
            }||]
    , tokA @"##" <^> varA @"expr_block_body"
        <:> \(kblock :* expr :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(kblock), $$(expr))
            ||]
    , varA @"expr_atomic"
        <:> \(expr :* HNil) ->
            expr
    ]

rPatAtomics0 :: RuleExpr (Bag.T (Ast.Pat AstParsed.T), Maybe Spanned.Span)
rPatAtomics0 = ruleExpr
    [ varA @"pat_atomic" <^> varA @"pat_atomic*"
        <:> \(pat :* pats :* HNil) ->
            [||case $$(pats) of { (pats, msPats) ->
                ( cons $$(pat) pats
                , Just do Spanned.sp do $$(pat) AstParsed.:<< msPats
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

rExprs0WithComma :: RuleExpr ([Ast.Expr AstParsed.T], Maybe Spanned.Span)
rExprs0WithComma = ruleExpr
    [ tokA @"," <^> varA @"(expr ',')* expr?"
        <:> \(kcomma :* exprs :* HNil) ->
            [||case $$(exprs) of { (exprs, msExprs) ->
                ( otoList exprs
                , Just do Spanned.sp do $$(kcomma) AstParsed.:<< msExprs
                )
            }||]
    , varA @"(expr ',')* expr?"
        <:> \(exprs :* HNil) ->
            [||case $$(exprs) of { (exprs, msExprs) ->
                ( otoList exprs
                , msExprs
                )
            }||]
    ]

rExprs0 :: RuleExpr (Bag.T (Ast.Expr AstParsed.T), Maybe Spanned.Span)
rExprs0 = ruleExpr
    [ varA @"expr" <^> tokA @"," <^> varA @"(expr ',')* expr?"
        <:> \(expr :* kcomma :* exprs :* HNil) ->
            [||case $$(exprs) of { (exprs, msExprs) ->
                ( cons $$(expr) exprs
                , Just do Spanned.sp ($$(expr), $$(kcomma) AstParsed.:<< msExprs)
                )
            }||]
    , varA @"expr"
        <:> \(expr :* HNil) ->
            [||
                ( pure $$(expr)
                , Just do Spanned.sp $$(expr)
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

rExprAtomic :: RuleExpr (Ast.Expr AstParsed.T)
rExprAtomic = ruleExpr
    [ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> \(kparenl :* expr :* kparenr :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(kparenl), $$(expr), $$(kparenr))
            ||]
    , varA @"expr_literal"
        <:> \(expr :* HNil) ->
            expr
    , varA @"con"
        <:> \(con :* HNil) ->
            [||case $$(con) of { (con, spCon) ->
                Ast.ExprCon con spCon
            }||]
    , varA @"var"
        <:> \(var :* HNil) ->
            [||case $$(var) of { (var, spVar) ->
                Ast.ExprVar var spVar
            }||]
    ]

rExprLiteral :: RuleExpr (Ast.Expr AstParsed.T)
rExprLiteral = ruleExpr
    [ varA @"literal"
        <:> \(lit :* HNil) ->
            [||Ast.ExprLit $$(lit) do
                Spanned.sp $$(lit)
            ||]
    , varA @"expr_interp_string"
        <:> \(parts :* HNil) ->
            [||case $$(parts) of { (parts, spParts) ->
                Ast.ExprInterpString parts spParts
            }||]
    , varA @"(" <^> varA @"expr_tuple_items" <^> varA @")"
        <:> \(kparenl :* items :* kparenr :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                Ast.ExprTuple items do
                    Spanned.sp ($$(kparenl), msItems AstParsed.:>> $$(kparenr))
            }||]
    , varA @"[" <^> varA @"expr_array_items" <^> varA @"]"
        <:> \(kbrackl :* items :* kbrackr :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                Ast.ExprArray items do
                    Spanned.sp ($$(kbrackl), msItems AstParsed.:>> $$(kbrackr))
            }||]
    , varA @"{" <^> varA @"expr_simplrecord_items" <^> varA @"}"
        <:> \(kbracel :* items :* kbracer :* HNil) ->
            [||case $$(items) of { (items, msItems) ->
                Ast.ExprRecord items do
                    Spanned.sp ($$(kbracel), msItems AstParsed.:>> $$(kbracer))
            }||]
    ]

rExprBlockBody :: RuleExpr (Ast.Expr AstParsed.T)
rExprBlockBody = ruleExpr
    [ varA @"{{" <^> varA @"expr_block_item" <^> varA @"}}"
        <:> \(kdbraceo :* expr :* kdbracec :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(kdbraceo), $$(expr), $$(kdbracec))
            ||]
    , varA @"{" <^> varA @"expr_block_item" <^> varA @"}"
        <:> \(kbraceo :* expr :* kbracec :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(kbraceo), $$(expr), $$(kbracec))
            ||]
    , varA @"imp_bo" <^> varA @"expr_block_item" <^> varA @"imp_bc"
        <:> \(impBo :* expr :* impBc :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp ($$(impBo), $$(expr), $$(impBc))
            ||]
    ]

rExprBlockItem :: RuleExpr (Ast.Expr AstParsed.T)
rExprBlockItem = ruleExpr
    [ varA @"lsemis?" <^> varA @"expr" <^> varA @"lsemis?"
        <:> \(mlsemis1 :* expr :* mlsemis2 :* HNil) ->
            [||Ast.ExprAnn $$(expr) do
                Spanned.sp do $$(mlsemis1) AstParsed.:>> $$(expr) AstParsed.:<< $$(mlsemis2)
            ||]
    ]

rExprInterpString :: RuleExpr (NonEmpty (Ast.InterpStringPart AstParsed.T), Spanned.Span)
rExprInterpString = ruleExpr
    [ varA @"interp_string_without_interp"
        <:> \(part :* HNil) ->
            [||
                ( pure $$(part)
                , Spanned.sp $$(part)
                )
            ||]
    , varA @"interp_string_start" <^> varA @"expr" <^> varA @"(interp_string_cont expr)* interp_string_end"
        <:> \(part :* expr :* parts :* HNil) ->
            [||case $$(part) of { (part, spPart) ->
                case $$(parts) of { (parts, spParts) ->
                    let exprPart = Ast.InterpStringExpr $$(expr) do
                            Spanned.sp $$(expr)
                    in
                        ( part :| exprPart : otoList parts
                        , Spanned.sp (part, exprPart, spParts)
                        )
                }
            }||]
    ]

rExprInterpStringStart :: RuleExpr (Ast.InterpStringPart AstParsed.T)
rExprInterpStringStart = ruleExpr
    [ tokA @"interp_string_start"
        <:> \(part :* HNil) ->
            [||case Spanned.unSpanned $$(part) of {
                Ast.LitInterpStringStart txt ->
                    Ast.InterpStringLit txt do
                        Spanned.sp $$(part)
                _ ->
                    error "unreachable"
            }||]
    ]

rExprInterpStringContParts :: RuleExpr (Bag.T (Ast.InterpStringPart AstParsed.T), Spanned.Span)
rExprInterpStringContParts = ruleExpr
    [ varA @"interp_string_cont" <^> varA @"expr" <^> varA @"(interp_string_cont expr)* interp_string_end"
        <:> \(part :* expr :* parts :* HNil) ->
            [||case $$(part) of { (part, spPart) ->
                case $$(parts) of { (parts, spParts) ->
                    let exprPart = Ast.InterpStringExpr $$(expr) do
                            Spanned.sp $$(expr)
                    in
                        ( cons part do cons exprPart parts
                        , Spanned.sp (part, exprPart, spParts)
                        )
                }
            }||]
    , varA @"interp_string_end"
        <:> \(part :* HNil) ->
            [||
                ( pure $$(part)
                , Spanned.sp $$(part)
                )
            ||]
    ]

rExprInterpStringCont :: RuleExpr (Ast.InterpStringPart AstParsed.T)
rExprInterpStringCont = ruleExpr
    [ tokA @"interp_string_cont"
        <:> \(part :* HNil) ->
            [||case Spanned.unSpanned $$(part) of {
                Ast.LitInterpStringCont txt ->
                    Ast.InterpStringLit txt do
                        Spanned.sp $$(part)
                _ ->
                    error "unreachable"
            }||]
    ]

rExprInterpStringEnd :: RuleExpr (Ast.InterpStringPart AstParsed.T)
rExprInterpStringEnd = ruleExpr
    [ tokA @"interp_string_end"
        <:> \(part :* HNil) ->
            [||case Spanned.unSpanned $$(part) of {
                Ast.LitInterpStringEnd txt ->
                    Ast.InterpStringLit txt do
                        Spanned.sp $$(part)
                _ ->
                    error "unreachable"
            }||]
    ]


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
