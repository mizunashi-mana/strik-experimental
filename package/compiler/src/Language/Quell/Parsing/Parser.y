{
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Quell.Parsing.Parser (
    parseProgram,
    parseType,
    parseExpr,
    parsePat,
    parseLiteral,
) where

import Language.Quell.Prelude

import qualified Prelude
import qualified Language.Quell.Type.Ast                        as Ast
import qualified Language.Quell.Type.Token                      as Token
import qualified Language.Quell.Parsing.Parser.Layout           as Layout
import qualified Language.Quell.Data.Bag                        as Bag
import qualified Language.Quell.Parsing.Spanned                 as Spanned
import qualified Language.Quell.Parsing.Parser.Runner           as Runner
import           Language.Quell.Parsing.Parser.AstParsed
}

%expect 0

%token
    '#case'         { S Token.KwCase }
    '#data'         { S Token.KwData }
    '#do'           { S Token.KwDo }
    '#in'           { S Token.KwIn }
    '#let'          { S Token.KwLet }
    '#letrec'       { S Token.KwLetrec }
    '#newtype'      { S Token.KwNewtype }
    '#of'           { S Token.KwOf }
    '#type'         { S Token.KwType }
    '#when'         { S Token.KwWhen }
    '#where'        { S Token.KwWhere }
    '#yield'        { S Token.KwYield }

    '->'        { S Token.SymArrow }
    '@'         { S Token.SymAt }
    ':'         { S Token.SymColon }
    '=>'        { S Token.SymDArrow }
    '='         { S Token.SymEqual }
    '\\/'       { S Token.SymForall }
    '\\'        { S Token.SymLambda }
    '<-'        { S Token.SymLeftArrow }
    '|'         { S Token.SymOr }
    '_'         { S Token.SymUnderscore }

    '`'         { S Token.SpBackquote }
    '##'        { S Token.SpBlock }
    '#@'        { S Token.SpTypeBlock }
    '['         { S Token.SpBrackOpen }
    ']'         { S Token.SpBrackClose }
    ','         { S Token.SpComma }
    '{'         { S Token.SpBraceOpen }
    '}'         { S Token.SpBraceClose }
    '{{'        { S Token.SpDBraceOpen }
    '}}'        { S Token.SpDBraceClose }
    '('         { S Token.SpParenOpen }
    ')'         { S Token.SpParenClose }
    ';'         { S Token.SpSemi }
    VOBRACE     { S Token.SpVBraceOpen }
    VCBRACE     { S Token.SpVBraceClose }
    VSEMI       { S Token.SpVSemi }

    CONID       { S (Token.IdConId _) }
    CONSYM      { S (Token.IdConSym _) }
    VARID       { S (Token.IdVarId _) }
    VARSYM      { S (Token.IdVarSym _) }

    BYTECHAR    { S (Token.LitByteChar _) }
    BYTESTRING  { S (Token.LitByteString _) }
    CHAR        { S (Token.LitChar _) }
    STRING      { S (Token.LitString _) }
    INTEGER     { S (Token.LitInteger _) }
    RATIONAL    { S (Token.LitRational _) }

    INTERP_STRING_WITHOUT_INTERP    { S (Token.LitInterpStringWithoutInterp _) }
    INTERP_STRING_START             { S (Token.LitInterpStringStart _) }
    INTERP_STRING_CONTINUE          { S (Token.LitInterpStringContinue _) }
    INTERP_STRING_END               { S (Token.LitInterpStringEnd _) }

%monad { Monad m }{ Runner.T m }{ >>= }{ return }
%lexer { lexer }{ S Token.EndOfSource }
%tokentype { Spanned.T Token.T }

%name parseProgram          program
%name parseType             type
%name parseExpr             expr
%name parsePat              pat
%name parseLiteral          literal
%%

program :: { Ast.Program C }
    : decl_body
    {
        Ast.Program
            {
                decls = otoList $1
            }
    }

decl_body :: { Bag.T (Ast.Decl C) }
    : lopen decl_items lclose   { $2 }

decl_items :: { Bag.T (Ast.Decl C) }
    : decl_items_semis decl_item    { $1 <> pure $2 }
    | decl_items_semis              { $1 }

decl_items_semis :: { Bag.T (Ast.Decl C) }
    : decl_items_semis decl_item lsemis     { $1 <> pure $2 }
    | {- empty -}                           { mempty }

decl_item :: { Ast.Decl C }
    : sig_item              { $1 }
    | type_decl             { $1 }
    | data_decl             { $1 }
    | val_decl              { $1 }


typesig_decl :: { Ast.Decl C }
    : '#type' declcon ':' type
    { spAnn ($1, $2, $3, $4) do Ast.DeclTypeSig (unS $2) $4 }

valsig_decl :: { Ast.Decl C }
    : declvar ':' type
    { spAnn ($1, $2, $3) do Ast.DeclValSig (unS $1) $3 }

consig_decl :: { Ast.Decl C }
    : declcon ':' type
    { spAnn ($1, $2, $3) do Ast.DeclConSig (unS $1) $3 }


type_decl :: { Ast.Decl C }
    : '#type' decltype '=' type type_decl_where
    {
        case $5 of { (ms5, ds) ->
            spAnn ($1, $2, $3, $4 :< ms5) do Ast.DeclType $2 $4 ds
        }
    }

type_decl_where :: { (Maybe Span, [Ast.Decl C]) }
    : '#where' type_decl_where_body
    {
        case $2 of { (ms2, ds) ->
            (Just do sp do $1 :< ms2, ds)
        }
    }
    | {- empty -}
    { (Nothing, []) }

type_decl_where_body :: { (Maybe Span, [Ast.Decl C]) }
    : lopen type_decl_where_items lclose
    {
        case $2 of { (ms2, ds) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList ds)
        }
    }

type_decl_where_items :: { MaySpBag (Ast.Decl C) }
    : type_decl_where_items_semis type_decl_where_item
    { maySpBagAppend $1 $2 $2 }
    | type_decl_where_items_semis
    { $1 }

type_decl_where_items_semis :: { MaySpBag (Ast.Decl C) }
    : type_decl_where_items_semis type_decl_where_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

type_decl_where_item :: { Ast.Decl C }
    : typesig_decl      { $1 }
    | type_decl         { $1 }


data_decl :: { Ast.Decl C }
    : '#data' declcon ':' type data_decl_where
    {
        case $5 of { (ms5, ds) ->
            spAnn ($1, $2, $3, $4 :< ms5) do Ast.DeclDataType (unS $2) (Just $4) ds
        }
    }
    | '#data' declcon data_decl_where
    {
        case $3 of { (ms3, ds) ->
            spAnn ($1, $2 :< ms3) do Ast.DeclDataType (unS $2) Nothing ds
        }
    }
    | '#data' decltype '=' alg_data_type type_decl_where
    {
        case $5 of { (ms5, ds) ->
            spAnn ($1, $2, $3, $4 :< ms5) do Ast.DeclAlgDataType $2 (unS $4) ds
        }
    }
    | '#newtype' decltype '=' type type_decl_where
    {
        case $5 of { (ms5, ds) ->
            spAnn ($1, $2, $3, $4 :< ms5) do Ast.DeclNewType $2 $4 ds
        }
    }

data_decl_where :: { (Maybe Span, [Ast.Decl C]) }
    : '#where' data_decl_body
    {
        case $2 of { (ms2, ds) ->
            (Just do sp do $1 :< ms2, ds)
        }
    }
    | {- empty -}
    { (Nothing, []) }

data_decl_body :: { (Maybe Span, [Ast.Decl C]) }
    : lopen data_decl_items lclose
    {
        case $2 of { (ms2, ds) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList ds)
        }
    }

data_decl_items :: { MaySpBag (Ast.Decl C) }
    : data_decl_items_semis data_decl_item  { maySpBagAppend $1 $2 $2 }
    | data_decl_items_semis                 { maySpBagEmpty }

data_decl_items_semis :: { MaySpBag (Ast.Decl C) }
    : data_decl_items_semis data_decl_item lsemis   { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}                                   { maySpBagEmpty }

data_decl_item :: { Ast.Decl C }
    : consig_decl       { $1 }

alg_data_type :: { S [Ast.ImplType C] }
    : '(' alg_data_type_items ')'   { spn ($1, $2, $3) do unS $2 }
    | '(' vbars ')'                 { spn ($1 :< $2, $3) [] }
    | alg_data_type_items           { $1 }

alg_data_type_items :: { S [Ast.ImplType C] }
    : alg_data_type_items_vbar impltype vbars
    {
        case $1 of { (ms1, ts1) ->
            let ts = otoList do snoc ts $2
            in spn (ms1 :> $2 :< $3) ts
        }
    }

alg_data_type_items_vbar :: { MaySpBag (Ast.ImplType C) }
    : alg_data_type_items_vbar impltype vbars1  { maySpBagAppend $1 ($2, $3) $2 }
    | vbars                                     { maySpBagEmptyWithSpan $1 }


val_decl :: { Ast.Decl C }
    : declvarexpr '=' expr val_decl_where
    {
        case $4 of { (ms4, ds) ->
            spAnn ($1, $2, $3 :< ms4) do Ast.DeclVal $1 $3 ds
        }
    }

val_bind :: { Ast.Decl C }
    : pat '=' expr val_decl_where
    {
        case $4 of { (ms4, ds) ->
            spAnn ($1, $2, $3 :< ms4) do Ast.DeclValBind $1 $3 ds
        }
    }

val_decl_where :: { (Maybe Span, [Ast.Decl C]) }
    : '#where' val_decl_where_body
    {
        case $2 of { (ms2, ds) ->
            (Just do sp do $1 :< ms2, ds)
        }
    }
    | {- empty -}
    { (Nothing, []) }

val_decl_where_body :: { (Maybe Span, [Ast.Decl C]) }
    : lopen val_decl_where_items lclose
    {
        case $2 of { (ms2, ds) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList ds)
        }
    }

val_decl_where_items :: { MaySpBag (Ast.Decl C) }
    : val_decl_where_items_semis val_decl_where_item
    { maySpBagAppend $1 $2 $2 }
    | val_decl_where_items_semis
    { $1 }

val_decl_where_items_semis :: { MaySpBag (Ast.Decl C) }
    : val_decl_where_items_semis val_decl_where_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

val_decl_where_item :: { Ast.Decl C }
    : let_bind_item     { $1 }


decltype :: { Ast.DeclType C }
    : declcon bind_vars
    {
        case $2 of { (ms2, vs) ->
            spAnn ($1 :< ms2) do Ast.DeclAppType (unS $1) vs
        }
    }
    | simple_bind_var_decl declconop simple_bind_var_decl
    { spAnn ($1, $2, $3) do Ast.DeclInfixType $1 (unS $2) $3 }

impltype :: { Ast.ImplType C }
    : con type_apps_list
    {
        let ts = otoList $2
        in spAnn ($1 :< ts) do Ast.ImplAppType (unS $1) ts
    }
    | type_qualified conop type_qualified
    { spAnn ($1, $2, $3) do Ast.ImplInfixType $1 (unS $2) $3 }

declvarexpr :: { Ast.DeclExpr C }
    : declvar bind_vars
    {
        case $2 of { (ms2, vs) ->
            spAnn ($1 :< ms2) do Ast.DeclAppExpr (unS $1) vs
        }
    }
    | simple_bind_var_decl declop simple_bind_var_decl
    {
        spAnn ($1, $2, $3) do Ast.DeclInfixExpr $1 (unS $2) $3
    }

simple_bind_var_decl :: { Ast.BindVar C }
    : simple_bind_var
    { spAnn $1 case unS $1 of (n, mt) -> Ast.BindVar n mt }


type :: { Ast.TypeExpr C }
    : '\\/' bind_vars '=>' type
    {
        case $2 of { (ms2, vs) ->
            spAnn ($1 :< ms2, $3, $4) do Ast.TypeForall vs $4
        }
    }
    | type_unit '->' type
    {
        case spAnn $2 do Ast.TypeVar Ast.primNameArrow of
            t2 -> spAnn ($1, $2, $3) do Ast.TypeInfix $1 t2 $3
    }
    | type_unit %shift
    { $1 }

type_unit :: { Ast.TypeExpr C }
    : type_infix %shift         { $1 }

type_infix :: { Ast.TypeExpr C }
    : type_infix type_op type_apps  { spAnn ($1, $2, $3) do Ast.TypeInfix $1 $2 $3 }
    | type_apps %shift              { $1 }

type_op :: { Ast.TypeExpr C }
    : consym                        { spAnn $1 do Ast.TypeCon do unS $1 }
    | var_sym_ext                   { spAnn $1 do Ast.TypeVar do unS $1 }
    | '`' type_qualified_op '`'     { spAnn ($1, $2, $3) do Ast.TypeAnn $2 }

type_qualified_op :: { Ast.TypeExpr C }
    : con_sym_ext   { spAnn $1 do Ast.TypeCon do unS $1 }
    | var_sym_ext   { spAnn $1 do Ast.TypeVar do unS $1 }
    | type_block    { $1 }

type_apps :: { Ast.TypeExpr C }
    : type_qualified type_apps_list %shift
    {
        case otoList $2 of
            [] ->
                $1
            xs0@(x:xs) ->
                spAnn ($1, x :| xs) do Ast.TypeApp $1 xs0
    }

type_apps_list :: { Bag.T (Ast.AppType C) }
    : type_apps_list type_app   { snoc $1 $2 }
    | {- empty -}               { mempty }

type_app :: { Ast.AppType C }
    : '@' type_qualified    { spAnn ($1, $2) do Ast.UnivAppType $2 }
    | '#@' type_block_body  { spAnn ($1, $2) do Ast.UnivAppType do unS $2 }
    | type_qualified        { spAnn $1 do Ast.AppType $1 }

type_qualified :: { Ast.TypeExpr C }
    : type_block            { $1 }

type_block :: { Ast.TypeExpr C }
    : '##' type_block_body  { spAnn ($1, $2) do Ast.TypeAnn do unS $2 }
    | type_atomic           { $1 }

type_atomic :: { Ast.TypeExpr C }
    : '(' type ':' type ')'
    { spAnn ($1, $2, $3, $4, $5) do Ast.TypeSig $2 $4 }
    | '(' type ')'
    { spAnn ($1, $2, $3) do Ast.TypeAnn $2 }
    | con
    { spAnn $1 do Ast.TypeCon do unS $1 }
    | var
    { spAnn $1 do Ast.TypeVar do unS $1 }
    | type_literal
    { $1 }

type_literal :: { Ast.TypeExpr C }
    : literal
    { spAnn $1 do Ast.TypeLit $1 }
    | '(' type_tuple_items ')'
    { spAnn ($1, $2, $3) do Ast.TypeTuple do otoList do unS $2 }
    | '[' type_array_items ']'
    {
        case $2 of { (ms2, ts) ->
            spAnn ($1 :< ms2, $3) do Ast.TypeArray do otoList ts
        }
    }
    | '{' type_simplrecord_items '}'
    {
        case $2 of { (ms2, ts) ->
            spAnn ($1 :< ms2, $3) do Ast.TypeRecord do otoList ts
        }
    }

type_block_body :: { S (Ast.TypeExpr C) }
    : lopen type_block_item lclose
    { spn ($1 :> $2 :< $3) do unS $2 }

type_block_item :: { S (Ast.TypeExpr C) }
    : type lsemis   { spn ($1 :< $2) $1 }
    | type          { spn $1 $1 }

type_tuple_items :: { S (Bag.T (Ast.TypeExpr C)) }
    : type_tuple_items_commas type ','  { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | type_tuple_items_commas type      { spn ($1, $2) do snoc (unS $1) $2 }

type_tuple_items_commas :: { S (Bag.T (Ast.TypeExpr C)) }
    : type_tuple_items_commas type ','  { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | type ','                          { spn ($1, $2) do pure $1 }

type_array_items :: { MaySpBag (Ast.TypeExpr C) }
    : type_array_items_commas type      { maySpBagAppend $1 $2 $2 }
    | type_array_items_commas           { $1 }

type_array_items_commas :: { MaySpBag (Ast.TypeExpr C) }
    : type_array_items_commas type ','  { maySpBagAppend $1 ($2, $3) $2 }
    | {- empty -}                       { maySpBagEmpty }

type_simplrecord_items :: { MaySpBag (Ast.Name, Ast.TypeExpr C) }
    : type_simplrecord_items_commas type_simplrecord_item
    { maySpBagAppend $1 $2 do unS $2 }
    | type_simplrecord_items_commas
    { $1 }

type_simplrecord_items_commas :: { MaySpBag (Ast.Name, Ast.TypeExpr C) }
    : type_simplrecord_items_commas type_simplrecord_item ','
    { maySpBagAppend $1 ($2, $3) do unS $2 }
    | {- empty -}
    { maySpBagEmpty }

type_simplrecord_item :: { S (Ast.Name, Ast.TypeExpr C) }
    : var ':' type      { spn ($1, $2, $3) (unS $1, $3) }


sig_item :: { Ast.Decl C }
    : typesig_decl      { $1 }
    | valsig_decl       { $1 }


expr :: { Ast.Expr C }
    : expr_unit ':' type        { spAnn ($1, $2, $3) do Ast.ExprSig $1 $3 }
    | expr_unit %shift          { $1 }

expr_unit :: { Ast.Expr C }
    : expr_infix %shift         { $1 }

expr_infix :: { Ast.Expr C }
    : expr_infix expr_op expr_apps  { spAnn ($1, $2, $3) do Ast.ExprInfix $1 $2 $3 }
    | expr_apps                     { $1 }

expr_op :: { Ast.Expr C }
    : consym                        { spAnn $1 do Ast.ExprCon do unS $1 }
    | var_sym_ext                   { spAnn $1 do Ast.ExprVar do unS $1 }
    | '`' expr_qualified_op '`'     { spAnn ($1, $2, $3) do Ast.ExprAnn $2 }

expr_qualified_op :: { Ast.Expr C }
    : con_sym_ext       { spAnn $1 do Ast.ExprCon do unS $1 }
    | var_sym_ext       { spAnn $1 do Ast.ExprVar do unS $1 }
    | expr_block        { $1 }

expr_apps :: { Ast.Expr C }
    : expr_apps_list %shift
    {
        case $1 of { (e, b) -> case otoList b of
            [] ->
                e
            xs0@(x:xs) ->
                spAnn (e, x :| xs) do Ast.ExprApp e xs0
        }
    }

expr_apps_list :: { (Ast.Expr C, Bag.T (Ast.AppExpr C)) }
    : expr_apps_list expr_app   { case $1 of (e, xs) -> (e, snoc xs $2) }
    | expr_qualified            { ($1, mempty) }

expr_app :: { Ast.AppExpr C }
    : '@' type_qualified        { spAnn ($1, $2) do Ast.UnivAppExpr $2 }
    | '#@' type_block_body      { spAnn ($1, $2) do Ast.UnivAppExpr do unS $2 }
    | expr_qualified            { spAnn $1 do Ast.AppExpr $1 }

expr_qualified :: { Ast.Expr C }
    : expr_block                { $1 }

expr_block :: { Ast.Expr C }
    : '\\' '#case' case_alt_body
    {
        case $3 of { (ms3, alts) ->
            spAnn ($1, $2 :< ms3) do Ast.ExprLambda alts
        }
    }
    | '\\' lambda_body -- conflict with expr
    { spAnn ($1, $2) do Ast.ExprLambda [$2] }
    | '#let' let_body -- conflict with expr
    {
        case unS $2 of { (ds, e) ->
            spAnn ($1, $2) do Ast.ExprLet ds e
        }
    }
    | '#letrec' let_body -- conflict with expr
    {
        case unS $2 of { (ds, e) ->
            spAnn ($1, $2) do Ast.ExprLetrec ds e
        }
    }
    | '#case' case_body
    {
        case unS $2 of { (es, alts) ->
            spAnn ($1, $2) do Ast.ExprCase es alts
        }
    }
    | '#do' do_body
    {
        case unS $2 of { (ss, e) ->
            spAnn ($1, $2) do Ast.ExprDo ss e
        }
    }
    | '##' expr_block_body
    { spAnn ($1, $2) do Ast.ExprAnn do unS $2 }
    | expr_atomic
    { $1 }

expr_atomic :: { Ast.Expr C }
    : '(' expr ')'                  { spAnn ($1, $2, $3) do Ast.ExprAnn $2 }
    | con                           { spAnn $1 do Ast.ExprCon do unS $1 }
    | var                           { spAnn $1 do Ast.ExprVar do unS $1 }
    | expr_literal                  { $1 }

expr_literal :: { Ast.Expr C }
    : literal
    { spAnn $1 do Ast.ExprLit $1 }
    | expr_interp_string
    { $1 }
    | '(' expr_tuple_items ')'
    { spAnn ($1, $2, $3) do Ast.ExprTuple do otoList do unS $2 }
    | '[' expr_array_items ']'
    {
        case $2 of { (ms2, es) ->
            spAnn ($1 :< ms2, $3) do Ast.ExprArray do otoList es
        }
    }
    | '{' expr_simplrecord_items '}'
    {
        case $2 of { (ms2, es) ->
            spAnn ($1 :< ms2, $3) do Ast.ExprRecord do otoList es
        }
    }

expr_block_body :: { S (Ast.Expr C) }
    : lopen expr_block_item lclose
    { spn ($1 :> $2 :< $3) do unS $2 }

expr_block_item :: { S (Ast.Expr C) }
    : expr lsemis   { spn ($1 :< $2) $1 }
    | expr          { spn $1 $1 }

expr_interp_string :: { Ast.Expr C }
    : interp_string_without_interp
    { spAnn $1 do Ast.ExprInterpString [$1] }
    | interp_string_start interp_string_expr expr_interp_string_conts interp_string_end
    {
        case otoList do cons $2 do snoc $3 $4 of { xs ->
            spAnn ($1 :| xs) do Ast.ExprInterpString do $1:xs
        }
    }

expr_interp_string_conts :: { Bag.T (Ast.InterpStringPart C) }
    : expr_interp_string_conts interp_string_cont interp_string_expr
    { snoc (snoc $1 $2) $3 }
    | {- empty -}
    { mempty }

interp_string_without_interp :: { Ast.InterpStringPart C }
    : INTERP_STRING_WITHOUT_INTERP
    {
        case unS $1 of
            Token.LitInterpStringWithoutInterp txt ->
                spAnn $1 do Ast.InterpStringLit txt
            _ ->
                error "unreachable"
    }

interp_string_start :: { Ast.InterpStringPart C }
    : INTERP_STRING_START
    {
        case unS $1 of
            Token.LitInterpStringStart txt ->
                spAnn $1 do Ast.InterpStringLit txt
            _ ->
                error "unreachable"
    }

interp_string_cont :: { Ast.InterpStringPart C }
    : INTERP_STRING_CONTINUE
    {
        case unS $1 of
            Token.LitInterpStringContinue txt ->
                spAnn $1 do Ast.InterpStringLit txt
            _ ->
                error "unreachable"
    }

interp_string_end :: { Ast.InterpStringPart C }
    : INTERP_STRING_END
    {
        case unS $1 of
            Token.LitInterpStringEnd txt ->
                spAnn $1 do Ast.InterpStringLit txt
            _ ->
                error "unreachable"
    }

interp_string_expr :: { Ast.InterpStringPart C }
    : expr      { spAnn $1 do Ast.InterpStringExpr $1 }

expr_tuple_items :: { S (Bag.T (Ast.Expr C)) }
    : expr_tuple_items_commas expr ','   { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | expr_tuple_items_commas expr       { spn ($1, $2) do snoc (unS $1) $2 }

expr_tuple_items_commas :: { S (Bag.T (Ast.Expr C)) }
    : expr_tuple_items_commas expr ','  { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | expr ','                          { spn ($1, $2) do pure $1 }

expr_array_items :: { MaySpBag (Ast.Expr C) }
    : expr_array_items_commas expr      { maySpBagAppend $1 $2 $2 }
    | expr_array_items_commas           { $1 }

expr_array_items_commas :: { MaySpBag (Ast.Expr C) }
    : expr_array_items_commas expr ','  { maySpBagAppend $1 ($2, $3) $2 }
    | {- empty -}                       { maySpBagEmpty }

expr_simplrecord_items :: { MaySpBag (Ast.Name, Ast.Expr C) }
    : expr_simplrecord_items_semis expr_simplrecord_item
    { maySpBagAppend $1 $2 do unS $2 }
    | expr_simplrecord_items_semis
    { $1 }

expr_simplrecord_items_semis :: { MaySpBag (Ast.Name, Ast.Expr C) }
    : expr_simplrecord_items_semis expr_simplrecord_item ','
    { maySpBagAppend $1 ($2, $3) do unS $2 }
    | {- empty -}
    { maySpBagEmpty }

expr_simplrecord_item :: { S (Ast.Name, Ast.Expr C) }
    : var '=' expr      { spn ($1, $2, $3) do (unS $1, $3) }


pat :: { Ast.Pat C }
    : pat_unit ':' type     { spAnn ($1, $2, $3) do Ast.PatSig $1 $3 }
    | pat_unit              { $1 }

pat_unit :: { Ast.Pat C }
    : pat_unit_list
    {
        let ps = otoList do unS $1
        in spAnn $1 do Ast.PatOr ps
    }

pat_unit_list :: { S (Bag.T (Ast.Pat C)) }
    : pat_unit_list '|' pat_infix %shift
    { spn ($1, $2, $3) do snoc (unS $1) $3 }
    | pat_infix %shift
    { spn $1 do pure $1 }

pat_infix :: { Ast.Pat C }
    : pat_infix conop_qualified pat_univ_apps
    { spAnn ($1, $2, $3) do Ast.PatInfix $1 (unS $2) $3 }
    | pat_univ_apps
    { $1 }

pat_univ_apps :: { Ast.Pat C }
    : pat_apps pat_univ_apps_args
    {
        case $2 of { (ms2, ts) ->
            spAnn ($1 :< ms2) do Ast.PatUnivApp $1 do otoList ts
        }
    }

pat_univ_apps_args :: { MaySpBag (Ast.TypeExpr C) }
    : pat_univ_apps_args pat_univ_app
    { maySpBagAppend $1 $2 do unS $2 }
    | {- empty -}
    { maySpBagEmpty }

pat_univ_app :: { S (Ast.TypeExpr C) }
    : '@' type_qualified        { spn ($1, $2) $2 }
    | '#@' type_block_body      { spAnn ($1, $2) do unS $2 }

pat_apps :: { Ast.Pat C }
    : pat_qualified pat_apps_args %shift
    {
        case $2 of { (ms2, ps) ->
            spAnn ($1 :< ms2) do Ast.PatApp (unS $1) do otoList ps
        }
    }
    | pat_qualified
    { $1 }

pat_apps_args :: { MaySpBag (Ast.AppPat C) }
    : pat_apps_args pat_app             { maySpBagAppend $1 $2 $2 }
    | {- empty -}                       { maySpBagEmpty }

pat_app :: { Ast.AppPat C }
    : pat_qualified     { spAnn $1 do Ast.AppPat $1 }
    | pat_univ_app      { spAnn $1 do Ast.UnivAppPat do unS $1 }

pat_qualified :: { Ast.Pat C }
    : pat_block     { $1 }

pat_block :: { Ast.Pat C }
    : '##' pat_block_body
    { spAnn ($1, $2) do Ast.PatAnn do unS $2 }
    | pat_atomic
    { $1 }

pat_atomic :: { Ast.Pat C }
    : '(' pat ')'           { spAnn ($1, $2, $3) do Ast.PatAnn $2 }
    | con                   { spAnn $1 do Ast.PatCon do unS $1 }
    | var %shift            { spAnn $1 do Ast.PatVar do unS $1 }
    | pat_literal           { $1 }

pat_literal :: { Ast.Pat C }
    : literal
    { spAnn $1 do Ast.PatLit $1 }
    | '(' pat_tuple_items ')'
    { spAnn ($1, $2, $3) do Ast.PatTuple do otoList do unS $2 }
    | '[' pat_array_items ']'
    {
        case $2 of { (ms2, ps) ->
            spAnn ($1 :< ms2, $3) do Ast.PatArray do otoList ps
        }
    }
    | '{' pat_simplrecord_items '}'
    {
        case $2 of { (ms2, ps) ->
            spAnn ($1 :< ms2, $3) do Ast.PatRecord do otoList ps
        }
    }

pat_block_body :: { S (Ast.Pat C) }
    : lopen pat_block_item lclose
    { spn ($1 :> $2 :< $3) do unS $2 }

pat_block_item :: { S (Ast.Pat C) }
    : pat lsemis    { spn ($1 :< $2) $1 }
    | pat           { spn $1 $1 }

pat_tuple_items :: { S (Bag.T (Ast.Pat C)) }
    : pat_tuple_items_commas pat ','    { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | pat_tuple_items_commas pat        { spn ($1, $2) do snoc (unS $1) $2 }

pat_tuple_items_commas :: { S (Bag.T (Ast.Pat C)) }
    : pat_tuple_items_commas pat ','    { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | pat ','                           { spn ($1, $2) do pure $1 }

pat_array_items :: { MaySpBag (Ast.Pat C) }
    : pat_array_items_commas pat        { maySpBagAppend $1 $2 $2 }
    | pat_array_items_commas            { $1 }

pat_array_items_commas :: { MaySpBag (Ast.Pat C) }
    : pat_array_items_commas pat ','    { maySpBagAppend $1 ($2, $3) $2 }
    | {- empty -}                       { maySpBagEmpty }

pat_simplrecord_items :: { MaySpBag (Ast.Name, Ast.Pat C) }
    : pat_simplrecord_items_semis pat_simplrecord_item
    { maySpBagAppend $1 $2 do unS $2 }
    | pat_simplrecord_items_semis
    { $1 }

pat_simplrecord_items_semis :: { MaySpBag (Ast.Name, Ast.Pat C) }
    : pat_simplrecord_items_semis pat_simplrecord_item ','
    { maySpBagAppend $1 ($2, $3) do unS $2 }
    | {- empty -}
    { maySpBagEmpty }

pat_simplrecord_item :: { S (Ast.Name, Ast.Pat C) }
    : var '=' pat       { spn ($1, $2, $3) (unS $1, $3) }


lambda_body :: { Ast.CaseAlt C }
    : lambda_pat_args guarded_alts
    {
        case $1 of { (ms1, ps) ->
            spAnn (ms1 :> $2) do Ast.CaseAlt (otoList ps) (unS $2)
        }
    }

lambda_pat_args :: { MaySpBag (Ast.Pat C) }
    : lambda_pat_args pat_atomic    { maySpBagAppend $1 $2 $2 }
    | {- empty -}                   { maySpBagEmpty }


let_body :: { S ([Ast.Decl C], Ast.Expr C) }
    : let_binds '#in' expr
    {
        case $1 of { (ms1, ds) ->
            spn (ms1 :> $2, $3) (ds, $3)
        }
    }

let_binds :: { (Maybe Span, [Ast.Decl C]) }
    : lopen let_bind_items lclose
    {
        case $2 of { (ms2, ds) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList ds)
        }
    }

let_bind_items :: { MaySpBag (Ast.Decl C) }
    : let_bind_items_semis let_bind_item
    { maySpBagAppend $1 $2 $2 }
    | let_bind_items_semis
    { $1 }

let_bind_items_semis :: { MaySpBag (Ast.Decl C) }
    : let_bind_items_semis let_bind_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

let_bind_item :: { Ast.Decl C }
    : sig_item                  { $1 }
    | type_decl                 { $1 }
    | data_decl                 { $1 }
    | val_bind                  { $1 } -- conflict with valsig_decl


case_body :: { S ([Ast.Expr C], [Ast.CaseAlt C]) }
    : case_exprs '#of' case_alt_body
    {
        case $1 of { (ms1, es) -> case $3 of { (ms3, alts) ->
            spn (ms1 :> $2 :< ms3) (otoList es, alts)
        } }
    }

case_exprs :: { MaySpBag (Ast.Expr C) }
    : case_exprs_commas expr    { maySpBagAppend $1 $2 $2 }
    | case_exprs_commas         { $1 }

case_exprs_commas :: { MaySpBag (Ast.Expr C) }
    : case_exprs_commas expr ','
    { maySpBagAppend $1 ($2, $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

case_alt_body :: { (Maybe Span, [Ast.CaseAlt C]) }
    : lopen case_alt_items lclose
    {
        case $2 of { (ms2, alts) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList alts)
        }
    }

case_alt_items :: { MaySpBag (Ast.CaseAlt C) }
    : case_alt_items_semis case_alt_item    { maySpBagAppend $1 $2 $2 }
    | case_alt_items_semis                  { $1 }

case_alt_items_semis :: { MaySpBag (Ast.CaseAlt C) }
    : case_alt_items_semis case_alt_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

case_alt_item :: { Ast.CaseAlt C }
    : case_pats guarded_alts
    {
        case $1 of { (ms1, ps) ->
            spAnn (ms1 :> $2) do Ast.CaseAlt (otoList ps) (unS $2)
        }
    }

case_pats :: { MaySpBag (Ast.Pat C) }
    : case_pats_commas pat_unit     { maySpBagAppend $1 $2 $2 }
    | case_pats_commas              { $1 }

case_pats_commas :: { MaySpBag (Ast.Pat C) }
    : case_pats_commas pat ','      { maySpBagAppend $1 ($2, $3) $2 }
    | {- empty -}                   { maySpBagEmpty }

guarded_alts :: { S [Ast.GuardedAlt C] }
    : '->' expr
    {
        let alt = spAnn $2 do Ast.GuardedAlt Nothing $2
        in spn ($1, alt) [alt]
    }
    | '#when' guarded_alt_body
    {
        case $2 of { (ms2, alts) ->
            spn ($1 :< ms2) alts
        }
    }

guarded_alt_body :: { (Maybe Span, [Ast.GuardedAlt C]) }
    : lopen guarded_alt_items lclose
    {
        case $2 of { (ms2, alts) ->
            let ms = maySp [fmap sp $1, ms2, fmap sp $3]
            in (ms, otoList alts)
        }
    }

guarded_alt_items :: { MaySpBag (Ast.GuardedAlt C) }
    : guarded_alt_items_semis guarded_alt_item
    { maySpBagAppend $1 $2 $2 }
    | guarded_alt_items_semis
    { $1 }

guarded_alt_items_semis :: { MaySpBag (Ast.GuardedAlt C) }
    : guarded_alt_items_semis guarded_alt_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

guarded_alt_item :: { Ast.GuardedAlt C }
    : guard_qual '->' expr
    { spAnn ($1 :> $2, $3) do Ast.GuardedAlt $1 $3 }

guard_qual :: { Maybe (Ast.Expr C) }
    : expr_unit         { Just $1 }


do_body :: { S ([Ast.DoStmt C], Ast.Expr C) }
    : lopen do_stmt_items lclose        { spn ($1 :> $2 :< $3) do unS $2 }

do_stmt_items :: { S ([Ast.DoStmt C], Ast.Expr C) }
    : do_stmt_items_semis do_yield_item lsemis
    {
        case $1 of { (ms1, ss) ->
            spn (ms1 :> $2 :< $3) do (otoList ss, unS $2)
        }
    }
    | do_stmt_items_semis do_yield_item
    {
        case $1 of { (ms1, ss) ->
            spn (ms1 :> $2) do (otoList ss, unS $2)
        }
    }

do_stmt_items_semis :: { MaySpBag (Ast.DoStmt C) }
    : do_stmt_items_semis do_stmt_item lsemis
    { maySpBagAppend $1 ($2 :< $3) $2 }
    | {- empty -}
    { maySpBagEmpty }

do_stmt_item :: { Ast.DoStmt C }
    : pat '<-' expr val_decl_where
    {
        case $4 of { (ms4, ds) ->
            spAnn ($1, $2, $3 :< ms4) do Ast.DoStmtMonBind $1 $3 ds
        }
    }
    | pat '=' expr val_decl_where
    {
        case $4 of { (ms4, ds) ->
            spAnn ($1, $2, $3 :< ms4) do Ast.DoStmtBind $1 $3 ds
        }
    }
    | '#letrec' let_binds
    {
        case $2 of { (ms2, ds) ->
            spAnn ($1 :< ms2) do Ast.DoStmtLetrec ds
        }
    }

do_yield_item :: { S (Ast.Expr C) }
    : '#yield' expr     { spn ($1, $2) $2 }


bind_var :: { Ast.BindVar C }
    : '@' simple_bind_var
    { spAnn ($1, $2) case unS $2 of (name, mayTy) -> Ast.UnivBindVar name mayTy }
    | '#@' block_bind_var
    { spAnn ($1, $2) case unS $2 of (name, mayTy) -> Ast.UnivBindVar name mayTy }
    | simple_bind_var
    { spAnn $1 case unS $1 of (name, mayTy) -> Ast.BindVar name mayTy }
    | '##' block_bind_var
    { spAnn ($1, $2) case unS $2 of (name, mayTy) -> Ast.BindVar name mayTy }

simple_bind_var :: { S (Ast.Name, Maybe (Ast.TypeExpr C)) }
    : var_id_ext
    { spn $1 (unS $1, Nothing) }
    | '(' var_id_ext ':' type ')'
    { spn ($1, $2, $3, $4, $5)  (unS $2, Just $4) }

block_bind_var :: { S (Ast.Name, Maybe (Ast.TypeExpr C)) }
    : lopen block_bind_var_items lclose
    { spn ($1 :> $2 :< $3) do unS $2 }

block_bind_var_items :: { S (Ast.Name, Maybe (Ast.TypeExpr C)) }
    : block_bind_var_item lsemis
    { spn ($1 :< $2) do unS $1 }

block_bind_var_item :: { S (Ast.Name, Maybe (Ast.TypeExpr C)) }
    : var_id_ext
    { spn $1 (unS $1, Nothing) }
    | var_id_ext ':' type
    { spn ($1, $2, $3)  (unS $1, Just $3) }

con_qualified :: { S Ast.Name }
    : con       { $1 }

conop_qualified :: { S Ast.Name }
    : conop     { $1 }

con :: { S Ast.Name }
    : con_id_ext            { $1 }
    | '(' con_sym_ext ')'   { spn ($1, $2, $3) do unS $2 }

conop :: { S Ast.Name }
    : con_sym_ext           { $1 }
    | '`' con_sym_ext '`'   { spn ($1, $2, $3) do unS $2 }
    | '`' con_id_ext '`'    { spn ($1, $2, $3) do unS $2 }

var :: { S Ast.Name }
    : var_id_ext            { $1 }
    | '(' var_sym_ext ')'   { spn ($1, $2, $3) do unS $2 }

op :: { S Ast.Name }
    : var_sym_ext               { $1 }
    | '`' var_sym_ext '`'       { spn ($1, $2, $3) do unS $2 }
    | '`' var_id_ext '`'        { spn ($1, $2, $3) do unS $2 }

con_id_ext :: { S Ast.Name }
    : conid             { $1 }
    | '(' ')'           { spn $1 Ast.primNameUnit }

con_sym_ext :: { S Ast.Name }
    : consym            { $1 }
    | '->'              { spn $1 Ast.primNameArrow }

var_id_ext :: { S Ast.Name }
    : varid     { $1 }
    | '_'       { spn $1 Ast.primNameWildcard }

var_sym_ext :: { S Ast.Name }
    : varsym    { $1 }


declcon :: { S Ast.Name }
    : con
    { if
        | isExtName do unS $1 -> undefined
        | otherwise -> $1
    }

declconop :: { S Ast.Name }
    : conop
    { if
        | isExtName do unS $1 -> undefined
        | otherwise -> $1
    }

declvar :: { S Ast.Name }
    : var
    { if
        | isExtName do unS $1 -> undefined
        | otherwise -> $1
    }

declop :: { S Ast.Name }
    : op
    { if
        | isExtName do unS $1 -> undefined
        | otherwise -> $1
    }


lopen :: { Maybe Span }
    : lopen VSEMI   { $1 }
    | '{'           { Just do sp $1 }
    | '{{'          { Just do sp $1 }
    | VOBRACE       { Nothing }

lclose :: { Maybe Span }
    : '}'           { Just do sp $1 }
    | '}}'          { Just do sp $1 }
    | VCBRACE       { Nothing }
    | error         { Nothing }

lsemis :: { Maybe Span }
    : lsemis semi   { $1 <> $2 }
    | semi          { $1 }

semi :: { Maybe Span }
    : ';'           { Just do sp $1 }
    | VSEMI         { Nothing }

vbars1 :: { Span }
    : vbars vbar    { sp do $1 :> $2 }

vbars :: { Maybe Span }
    : vbars vbar            { $1 <> Just $2 }
    | {- empty -} %shift    { Nothing }

vbar :: { Span }
    : '|'   { sp $1 }


literal :: { Ast.Lit C }
    : BYTECHAR
    {
        spAnn $1 case unS $1 of
            Token.LitByteChar w -> Ast.LitByteChar w
            _                   -> error "unreachable"
    }
    | BYTESTRING
    {
        spAnn $1 case unS $1 of
            Token.LitByteString s   -> Ast.LitByteString s
            _                       -> error "unreachable"
    }
    | CHAR
    {
        spAnn $1 case unS $1 of
            Token.LitChar c -> Ast.LitChar c
            _               -> error "unreachable"
    }
    | STRING
    {
        spAnn $1 case unS $1 of
            Token.LitString s   -> Ast.LitString s
            _                   -> error "unreachable"
    }
    | INTEGER
    {
        spAnn $1 case unS $1 of
            Token.LitInteger i  -> Ast.LitInteger i
            _                   -> error "unreachable"
    }
    | RATIONAL
    {
        spAnn $1 case unS $1 of
            Token.LitRational r -> Ast.LitRational r
            _                   -> error "unreachable: expected rational literal"
    }

bind_vars :: { (Maybe Span, [Ast.BindVar C]) }
    : bind_vars_list
    {
        case $1 of { (ms, vs) ->
            (ms, otoList vs)
        }
    }

bind_vars_list :: { MaySpBag (Ast.BindVar C) }
    : bind_vars_list bind_var   { maySpBagAppend $1 $2 $2 }
    | {- empty -}               { maySpBagEmpty }


conid :: { S Ast.Name }
    : CONID
    {
        $1 <&> \case
            Token.IdConId n     -> n
            _                   -> error "unreachable"
    }

consym :: { S Ast.Name }
    : CONSYM
    {
        $1 <&> \case
            Token.IdConSym n    -> n
            _                   -> error "unreachable"
    }

varid :: { S Ast.Name }
    : VARID
    {
        $1 <&> \case
            Token.IdVarId n     -> n
            _                   -> error "unreachable"
    }

varsym :: { S Ast.Name }
    : VARSYM
    {
        $1 <&> \case
            Token.IdVarSym n    -> n
            _                   -> error "unreachable"
    }
{
parseProgram :: Monad m => Runner.T m (Ast.Program C)
parseType :: Monad m => Runner.T m (Ast.TypeExpr C)
parseExpr :: Monad m => Runner.T m (Ast.Expr C)
parsePat :: Monad m => Runner.T m (Ast.Pat C)
parseLiteral :: Monad m => Runner.T m (Ast.Lit C)

type Span = Spanned.Span
type S = Spanned.T

pattern S :: a -> S a
pattern S x <- Spanned.Spanned
    {
        getSpan = _,
        unSpanned = x
    }

unS :: S a -> a
unS sx = Spanned.unSpanned sx

type MaySpBag a = (Maybe Span, Bag.T a)

maySpBagAppend :: SpannedBuilder s => MaySpBag a -> s -> a -> MaySpBag a
maySpBagAppend (ms1, m) s2 x = (Just do sp (ms1 :> s2), snoc m x)

maySpBagEmpty :: MaySpBag a
maySpBagEmpty = (Nothing, mempty)

maySpBagEmptyWithSpan :: Maybe Span -> MaySpBag a
maySpBagEmptyWithSpan ms = (ms, mempty)


type C = AstParsed


mkName :: StringLit -> Ast.Name
mkName s = Ast.mkName do text s

isExtName :: Ast.Name -> Bool
isExtName n = any (== n)
    [
        Ast.primNameUnit,
        Ast.primNameArrow,
        Ast.primNameWildcard
    ]

lexer :: Monad m => (S Token.T -> Runner.T m a) -> Runner.T m a
lexer = Runner.lexer

happyError :: Monad m => Runner.T m a
happyError = Runner.parseError
}
