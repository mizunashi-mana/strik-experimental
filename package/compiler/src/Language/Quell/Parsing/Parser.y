{
module Language.Quell.Parsing.Parser where

import Language.Quell.Prelude

import qualified Prelude
import qualified Language.Quell.Type.Ast                        as Ast
import qualified Language.Quell.Type.Token                      as Token
import qualified Language.Quell.Parsing.Parser.Layout           as Layout
import qualified Language.Quell.Data.Bag                        as Bag
import qualified Language.Quell.Parsing.Spanned                 as Spanned
import qualified Language.Quell.Parsing.Runner                  as Runner
import           Language.Quell.Parsing.Parser.AstParsed
}

%expect 0

%token
    'case'          { S Token.KwCase }
    'data'          { S Token.KwData }
    'do'            { S Token.KwDo }
    'in'            { S Token.KwIn }
    'let'           { S Token.KwLet }
    'letrec'        { S Token.KwLetrec }
    'newtype'       { S Token.KwNewtype }
    'of'            { S Token.KwOf }
    'rec'           { S Token.KwRec }
    'use'           { S Token.KwUse }
    'type'          { S Token.KwType }
    'when'          { S Token.KwWhen }
    'where'         { S Token.KwWhere }
    '_'             { S Token.KwUnderscore }

    '->'        { S Token.SymArrow }
    '@'         { S Token.SymAt }
    ':'         { S Token.SymColon }
    '=>'        { S Token.SymDArrow }
    '='         { S Token.SymEqual }
    '\\/'       { S Token.SymForall }
    '\\'        { S Token.SymLambda }
    '<-'        { S Token.SymLeftArrow }
    '|'         { S Token.SymOr }

    '`'         { S Token.SpBackquote }
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

%monad { Runner.T }{ >>= }{ return }
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
    | type_decl             { undefined }
    | data_decl             { undefined }
    | val_decl              { undefined }


typesig_decl :: { Ast.TypeSigDecl C }
    : 'type' declcon ':' type
    { spAnn ($1, $2, $3, $4) do Ast.TypeSigDecl (unS $2) $4 }

valsig_decl :: { Ast.ValSigDecl C }
    : var ':' type -- declvar ':' type
    { spAnn ($1, $2, $3) do Ast.ValSigDecl (unS $1) $3 }

consig_decl :: { Ast.ConSigDecl C }
    : declcon ':' type
    { spAnn ($1, $2, $3) do Ast.ConSigDecl (unS $1) $3 }


type_decl :: { () }
    : 'type' decltype '=' type type_decl_where    { () }

type_decl_where :: { () }
    : 'where' type_decl_where_body  { () }
    | {- empty -}                   { () }

type_decl_where_body :: { () }
    : lopen type_decl_where_items lclose    { () }

type_decl_where_items :: { () }
    : type_decl_where_items_semis type_decl_where_item  { () }
    | type_decl_where_items_semis                       { () }

type_decl_where_items_semis :: { () }
    : type_decl_where_items_semis type_decl_where_item lsemis   { () }
    | {- empty -}                                               { () }

type_decl_where_item :: { () }
    : typesig_decl      { () }
    | type_decl         { () }


data_decl :: { () }
    : 'data' declcon ':' type data_decl_where                   { () }
    | 'data' declcon data_decl_where                            { () }
    | 'data' decltype '=' alg_data_type type_decl_where         { () }
    | 'newtype' decltype '=' type type_decl_where               { () }

data_decl_where :: { () }
    : 'where' data_decl_body    { () }
    | {- empty -}               { () }

data_decl_body :: { () }
    : lopen data_decl_items lclose  { () }

data_decl_items :: { () }
    : data_decl_items_semis data_decl_item  { () }
    | data_decl_items_semis                 { () }

data_decl_items_semis :: { () }
    : data_decl_items_semis data_decl_item lsemis   { () }
    | {- empty -}                                   { () }

data_decl_item :: { () }
    : consig_decl       { () }

alg_data_type :: { () }
    : '(' alg_data_type_items ')'   { () }
    | alg_data_type_items           { () }

alg_data_type_items :: { () }
    : alg_data_type_items_vbar impltype '|'     { () }
    | alg_data_type_items_vbar impltype         { () }

alg_data_type_items_vbar :: { () }
    : alg_data_type_items_vbar impltype '|'     { () }
    | '|'                                       { () }
    | {- empty -} %shift                        { () }


val_decl :: { () }
    : declvarexpr '=' expr val_decl_where     { () }

val_bind :: { () }
    : pat '=' expr val_decl_where       { () }

val_decl_where :: { () }
    : 'where' val_decl_where_body   { () }
    | {- empty -}                   { () }

val_decl_where_body :: { () }
    : lopen val_decl_where_items lclose { () }

val_decl_where_items :: { () }
    : val_decl_where_items_semis val_decl_where_item    { () }
    | val_decl_where_items_semis                        { () }

val_decl_where_items_semis :: { () }
    : val_decl_where_items_semis val_decl_where_item lsemis { () }
    | {- empty -}                                           { () }

val_decl_where_item :: { () }
    : let_bind_item     { () }


decltype :: { () }
    : declcon bind_vars                         { () }
    | simple_bind_var declconop simple_bind_var { () }

impltype :: { () }
    : con type_qualified                        { () }
    | type_qualified conop type_qualified       { () }

declvarexpr :: { () }
    : declvar bind_vars                         { () }
    | simple_bind_var declop simple_bind_var    { () }


type :: { Ast.TypeExpr C }
    : '\\/' bind_vars '=>' type     { undefined }
    | type_unit '->' type           { undefined }
    | type_unit %shift              { $1 }

type_unit :: { Ast.TypeExpr C }
    : type_infix %shift         { $1 }

type_infix :: { Ast.TypeExpr C }
    : type_infix type_op type_apps %shift   { spAnn ($1, $2, $3) do Ast.TypeInfix $1 $2 $3 }
    | type_apps %shift                      { $1 }

type_op :: { Ast.TypeExpr C }
    : consym                        { spAnn $1 do Ast.TypeCon do unS $1 }
    | var_sym_ext                   { spAnn $1 do Ast.TypeVar do unS $1 }
    | '`' type_qualified_op '`'     { spAnn ($1, $2, $3) do Ast.TypeAnn $2 }

type_qualified_op :: { Ast.TypeExpr C }
    : con_sym_ext   { spAnn $1 do Ast.TypeCon do unS $1 }
    | var_sym_ext   { spAnn $1 do Ast.TypeVar do unS $1 }
    | type_block    { $1 }

type_apps :: { Ast.TypeExpr C }
    : type_apps_list %shift
    {
        case $1 of
            (t, b) -> case otoList b of
                [] ->
                    t
                xs0@(x:xs) ->
                    spAnn (t, x :| xs) do Ast.TypeApp t xs0
    }

type_apps_list :: { (Ast.TypeExpr C, Bag.T (Ast.AppType C)) }
    : type_apps_list type_app   { case $1 of (t, xs) -> (t, snoc xs $2) }
    | type_qualified            { ($1, mempty) }

type_app :: { Ast.AppType C }
    : '@' type_qualified    { spAnn ($1, $2) do Ast.UnivAppType $2 }
    | type_qualified        { spAnn $1 do Ast.AppType $1 }

type_qualified :: { Ast.TypeExpr C }
    : type_block            { $1 }

type_block :: { Ast.TypeExpr C }
    : type_atomic           { $1 }

type_atomic :: { Ast.TypeExpr C }
    : '(' type ':' type ')'
    { spAnn ($1, $2, $3, $4, $5) do Ast.TypeSig $2 $4 }
    | '(' type ')'                      { spAnn ($1, $2, $3) do Ast.TypeAnn $2 }
    | con                               { spAnn $1 do Ast.TypeCon do unS $1 }
    | var                               { spAnn $1 do Ast.TypeVar do unS $1 }
    | literal                           { spAnn $1 do Ast.TypeLit $1 }
    | '(' type_tuple_items ')'
    { spAnn ($1, $2, $3) do Ast.TypeTuple do otoList do unS $2 }
    | '[' type_array_items ']'
    {
        case $2 of
            Nothing ->
                spAnn ($1, $3) do Ast.TypeArray []
            Just sts ->
                spAnn ($1, sts, $3) do Ast.TypeArray do otoList do unS sts
    }
    | '{' type_simplrecord_items '}'
    {
        case $2 of
            Nothing ->
                spAnn ($1, $3) do Ast.TypeRecord []
            Just sts ->
                spAnn ($1, sts, $3) do Ast.TypeRecord do otoList do unS sts
    }
type_tuple_items :: { S (Bag.T (Ast.TypeExpr C)) }
    : type_tuple_items_commas type ','  { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | type_tuple_items_commas type      { spn ($1, $2) do snoc (unS $1) $2 }

type_tuple_items_commas :: { S (Bag.T (Ast.TypeExpr C)) }
    : type_tuple_items_commas type ','  { spn ($1, $2, $3) do snoc (unS $1) $2 }
    | type ','                          { spn ($1, $2) do pure $1 }

type_array_items :: { Maybe (S (Bag.T (Ast.TypeExpr C))) }
    : type_array_items_commas type
    {
        case $1 of
            Nothing ->
                Just do spn $2 do pure $2
            Just sts ->
                Just do spn (sts, $2) do snoc (unS sts) $2
    }
    | type_array_items_commas   { $1 }

type_array_items_commas :: { Maybe (S (Bag.T (Ast.TypeExpr C))) }
    : type_array_items_commas type ','
    {
        case $1 of
            Nothing ->
                Just do spn ($2, $3) do pure $2
            Just sts ->
                Just do spn (sts, $2, $3) do snoc (unS sts) $2
    }
    | {- empty -}               { Nothing }

type_simplrecord_items :: { Maybe (S (Bag.T (Ast.Name, Ast.TypeExpr C))) }
    : type_simplrecord_items_commas type_simplrecord_item
    {
        case $1 of
            Nothing ->
                Just do spn $2 do pure do unS $2
            Just sts ->
                Just do spn (sts, $2) do snoc (unS sts) do unS $2
    }
    | type_simplrecord_items_commas                         { $1 }

type_simplrecord_items_commas :: { Maybe (S (Bag.T (Ast.Name, Ast.TypeExpr C))) }
    : type_simplrecord_items_commas type_simplrecord_item ','
    {
        case $1 of
            Nothing ->
                Just do spn ($2, $3) do pure do unS $2
            Just sts ->
                Just do spn (sts, $2, $3) do snoc (unS sts) do unS $2
    }
    | {- empty -}       { Nothing }

type_simplrecord_item :: { S (Ast.Name, Ast.TypeExpr C) }
    : var ':' type      { spn ($1, $2, $3) (unS $1, $3) }


sig_item :: { Ast.Decl C }
    : typesig_decl      { spAnn $1 do Ast.DeclTypeSig $1 }
    | valsig_decl       { spAnn $1 do Ast.DeclValSig $1 }


expr :: { Ast.Expr C }
    : expr_unit ':' type        { spAnn ($1, $2, $3) do Ast.ExprSig $1 $3 }
    | expr_unit %shift          { $1 }

expr_unit :: { Ast.Expr C }
    : expr_infix %shift         { $1 }

expr_infix :: { Ast.Expr C }
    : expr_infix expr_op expr_apps %shift   { spAnn ($1, $2, $3) do Ast.ExprInfix $1 $2 $3 }
    | expr_apps %shift                      { $1 }

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
        case $1 of
            (e, b) -> case otoList b of
                [] ->
                    e
                xs0@(x:xs) ->
                    spAnn (e, x :| xs) do Ast.ExprApp e xs0
    }

expr_apps_list :: { (Ast.Expr C, Bag.T (Ast.AppExpr C)) }
    : expr_apps_list expr_app   { case $1 of (e, xs) -> (e, snoc xs $2) }
    | expr_qualified            { ($1, mempty) }

expr_app :: { Ast.AppExpr C }
    : '@' type_qualified        { spAnn ($1, $2) do Ast.UnivAppExpr $2 }
    | expr_qualified            { spAnn $1 do Ast.AppExpr $1 }

expr_qualified :: { Ast.Expr C }
    : expr_block                { $1 }

expr_block :: { Ast.Expr C }
    : '\\' 'case' case_alt_body             { undefined }
    | '\\' 'when' guarded_alt_body          { undefined }
    | '\\' lambda_body                      { undefined } -- conflict with expr
    | 'let' let_body                        { undefined } -- conflict with expr
    | 'letrec' let_body                     { undefined } -- conflict with expr
    | 'case' case_body                      { undefined }
    | 'do' do_body                          { undefined }
    | expr_atomic                           { $1 }

expr_atomic :: { Ast.Expr C }
    : '(' expr ')'                  { spAnn ($1, $2, $3) do Ast.ExprAnn $2 }
    | con                           { spAnn $1 do Ast.ExprCon do unS $1 }
    | var                           { spAnn $1 do Ast.ExprVar do unS $1 }
    | expr_literal                  { $1 }

expr_literal :: { Ast.Expr C }
    : literal                           { spAnn $1 do Ast.ExprLit $1 }
    | expr_interp_string                { $1 }
    | '(' expr_tuple_items ')'          { undefined }
    | '[' expr_array_items ']'          { undefined }
    | '{' expr_simplrecord_items '}'    { undefined }

expr_interp_string :: { Ast.Expr C }
    : interp_string_without_interp
    { spAnn $1 do Ast.ExprInterpString [$1] }
    | interp_string_start interp_string_expr expr_interp_string_conts interp_string_end
    {
        case otoList do cons $2 do snoc $3 $4 of
            xs -> spAnn ($1 :| xs) do Ast.ExprInterpString do $1:xs
    }

expr_interp_string_conts :: { Bag.T (Ast.InterpStringPart C) }
    : expr_interp_string_conts interp_string_cont interp_string_expr
    { snoc (snoc $1 $2) $3 }
    | {- empty -}   { mempty }

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
    : expr  { spAnn $1 do Ast.InterpStringExpr $1 }

expr_tuple_items :: { () }
    : expr_tuple_items_commas expr ','   { () }
    | expr_tuple_items_commas expr       { () }

expr_tuple_items_commas :: { () }
    : expr_tuple_items_commas expr ','  { () }
    | expr ','                          { () }

expr_array_items :: { () }
    : expr_array_items_commas expr  { () }
    | expr_array_items_commas       { () }

expr_array_items_commas :: { () }
    : expr_array_items_commas expr ','  { () }
    | {- empty -}                       { () }

expr_simplrecord_items :: { () }
    : expr_simplrecord_items_semis expr_simplrecord_item    { () }
    | expr_simplrecord_items_semis                          { () }

expr_simplrecord_items_semis :: { () }
    : expr_simplrecord_items_semis expr_simplrecord_item ','    { () }
    | {- empty -}                                               { () }

expr_simplrecord_item :: { () }
    : var '=' type      { () }


pat :: { () }
    : pat_unit ':' type     { () }
    | pat_unit              { () }

pat_unit :: { () }
    : pat_unit '|' pat_infix        { () }
    | pat_infix                     { () }

pat_infix :: { () }
    : pat_infix pat_op pat_apps         { () }
    | pat_apps                          { () }

pat_op :: { () }
    : CONSYM                        { () }
    | var_sym_ext                   { () }
    | '`' pat_qualified_op '`'      { () }

pat_qualified_op :: { () }
    : con_sym_ext       { () }
    | var_sym_ext       { () }
    | pat_qualified     { () }

pat_apps :: { () }
    : pat_qualified pat_apps_args       { () }

pat_apps_args :: { () }
    : pat_apps_args pat_app             { () }
    | {- empty -}                       { () }

pat_app :: { () }
    : '@' pat_qualified         { () }
    | pat_qualified             { () }

pat_qualified :: { () }
    : pat_atomic     { () }

pat_atomic :: { () }
    : '(' pat ')'                   { () }
    | con                           { () }
    | var %shift                    { () }
    | pat_literal                   { () }

pat_literal :: { () }
    : literal                           { () }
    | '(' pat_tuple_items ')'           { () }
    | '[' pat_array_items ']'           { () }
    | '{' pat_simplrecord_items '}'     { () }

pat_tuple_items :: { () }
    : pat_tuple_items_commas pat ','    { () }
    | pat_tuple_items_commas pat        { () }

pat_tuple_items_commas :: { () }
    : pat_tuple_items_commas pat ','    { () }
    | pat ','                           { () }

pat_array_items :: { () }
    : pat_array_items_commas pat    { () }
    | pat_array_items_commas        { () }

pat_array_items_commas :: { () }
    : pat_array_items_commas pat ','    { () }
    | {- empty -}                       { () }

pat_simplrecord_items :: { () }
    : pat_simplrecord_items_semis pat_simplrecord_item      { () }
    | pat_simplrecord_items_semis                           { () }

pat_simplrecord_items_semis :: { () }
    : pat_simplrecord_items_semis pat_simplrecord_item ','      { () }
    | {- empty -}                                               { () }

pat_simplrecord_item :: { () }
    : var '=' pat       { () }


lambda_body :: { () }
    : lambda_pat_args '->' expr     { () }

lambda_pat_args :: { () }
    : lambda_pat_args pat_atomic    { () }
    | {- empty -}                   { () }


let_body :: { () }
    : let_binds 'in' expr           { () }

let_binds :: { () }
    : lopen let_bind_items lclose   { () }

let_bind_items :: { () }
    : let_bind_items_semis let_bind_item    { () }
    | let_bind_items_semis                  { () }

let_bind_items_semis :: { () }
    : let_bind_items_semis let_bind_item lsemis     { () }
    | {- empty -}                                   { () }

let_bind_item :: { () }
    : sig_item                  { () }
    | type_decl                 { () }
    | data_decl                 { () }
    | val_bind                  { () } -- conflict with valsig_decl


case_body :: { () }
    : case_exprs 'of' case_alt_body     { () }

case_exprs :: { () }
    : case_exprs_commas expr    { () }
    | case_exprs_commas         { () }

case_exprs_commas :: { () }
    : case_exprs_commas expr ','    { () }
    | {- empty -}                   { () }

case_alt_body :: { () }
    : lopen case_alt_items lclose       { () }

case_alt_items :: { () }
    : case_alt_items_semis case_alt_item    { () }
    | case_alt_items_semis                  { () }

case_alt_items_semis :: { () }
    : case_alt_items_semis case_alt_item lsemis     { () }
    | {- empty -}                                   { () }

case_alt_item :: { () }
    : case_pats guarded_alt     { () }

case_pats :: { () }
    : case_pats_commas pat_unit     { () }
    | case_pats_commas              { () }

case_pats_commas :: { () }
    : case_pats_commas pat ','      { () }
    | {- empty -}                   { () }

guarded_alt :: { () }
    : '->' expr                 { () }
    | 'when' guarded_alt_body   { () }

guarded_alt_body :: { () }
    : lopen guarded_alt_items lclose    { () }

guarded_alt_items :: { () }
    : guarded_alt_items_semis guarded_alt_item  { () }
    | guarded_alt_items_semis                   { () }

guarded_alt_items_semis :: { () }
    : guarded_alt_items_semis guarded_alt_item lsemis   { () }
    | {- empty -}                                       { () }

guarded_alt_item :: { () }
    : guard_qual '->' expr      { () }

guard_qual :: { () }
    : expr_unit         { () }


do_body :: { () }
    : lopen do_stmt_items lclose        { () }

do_stmt_items :: { () }
    : do_stmt_items_semis do_stmt_item lsemis   { () } -- do_stmt_items_semis expr lsemis
    | do_stmt_items_semis expr                  { () }

do_stmt_items_semis :: { () }
    : do_stmt_items_semis do_stmt_item lsemis   { () }
    | {- empty -}                               { () }

do_stmt_item :: { () }
    : expr                                      { () }
    | var_id_ext ':' type '<-' expr             { () }
    | var_id_ext '<-' expr                      { () }
    | var_id_ext ':' type '=' expr              { () }
    | var_id_ext '=' expr                       { () }
    | 'use' do_binds                            { () }
    | 'rec' let_binds                           { () }

do_binds :: { () }
    : lopen do_bind_items lclose    { () }

do_bind_items :: { () }
    : do_bind_items_semis do_bind_item  { () }
    | do_bind_items_semis               { () }

do_bind_items_semis :: { () }
    : do_bind_items_semis do_bind_item lsemis   { () }
    | {- empty -}                               { () }

do_bind_item :: { () }
    : let_bind_item     { () }
    | pat '<-' expr     { () }


bind_var :: { Ast.BindVar C }
    : '@' simple_bind_var
    { spAnn ($1, $2) case unS $2 of (name, mayTy) -> Ast.UnivBindVar name mayTy }
    | simple_bind_var
    { spAnn $1 case unS $1 of (name, mayTy) -> Ast.BindVar name mayTy }

simple_bind_var :: { S (Ast.Name, Maybe (Ast.TypeExpr C)) }
    : var_id_ext
    { spn $1 (unS $1, Nothing) }
    | '(' var_id_ext ':' type ')'
    {
        spn ($1, $2, $3, $5) -- FIXME: add $4
            (unS $2, Just undefined)
    }

con :: { S Ast.Name }
    : con_id_ext            { $1 }
    | '(' con_sym_ext ')'   { spn ($1, $2, $3) do unS $2 }

conop :: { S Ast.Name }
    : con_sym_ext           { $1 }
    | '`' con_sym_ext '`'   { spn ($1, $2, $3) do unS $2 }
    | '`' con_id_ext '`'    { spn ($1, $2, $3) do unS $2 }

var :: { S Ast.Name }
    : var_id_ext %shift         { $1 }
    | '(' var_sym_ext ')'       { spn ($1, $2, $3) do unS $2 }

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
    : lsemis semi   { $1 <:> $2 }
    | semi          { $1 }

semi :: { Maybe Span }
    : ';'           { Just do sp $1 }
    | VSEMI         { Nothing }


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
            _                   -> error "unreachable"
    }

bind_vars :: { Bag.T (Ast.BindVar C) }
    : bind_vars bind_var    { snoc $1 $2 }
    | {- empty -}           { mempty }


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
            Token.IdConSym n     -> n
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


type C = AstParsed


(<:>) :: Maybe Span -> Maybe Span -> Maybe Span
Nothing  <:> Nothing  = Nothing
msp1     <:> Nothing  = msp1
Nothing  <:> msp2     = msp2
Just sp1 <:> Just sp2 = Just do sp1 <> sp2

mkName :: StringLit -> Ast.Name
mkName s = Ast.mkName do text s

isExtName :: Ast.Name -> Bool
isExtName n = any (== n)
    [
        Ast.primNameUnit,
        Ast.primNameArrow,
        Ast.primNameWildcard
    ]

lexer = undefined

happyError = undefined
}
