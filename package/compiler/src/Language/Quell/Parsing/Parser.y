{
module Language.Quell.Parsing.Parser where

import Language.Quell.Prelude

import qualified Prelude
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Data.Bag as Bag
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Parsing.Runner as Runner
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


typesig_decl :: { S (Ast.TypeSigDecl C) }
    : 'type' declcon ':' type
    {
        spn ($1, $2, $3) do -- FIXME: add $4
            Ast.TypeSigDecl
                {
                    typeSigDeclCon = unS $2,
                    typeSigDeclType = undefined
                }
    }

valsig_decl :: { S (Ast.ValSigDecl C) }
    : var ':' type -- declvar ':' type
    {
        spn ($1, $2) do -- FIXME: add $3
            Ast.ValSigDecl
                {
                    valSigDeclVar = unS $1,
                    valSigDeclType = undefined
                }
    }

consig_decl :: { () }
    : con ':' type                  { () } -- declcon ':' type


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
    : 'data' declcon may_type_sig data_decl_where               { () }
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
    : alg_data_type_items_vbars impltype vbars  { () }
    | alg_data_type_items_vbars impltype        { () }

alg_data_type_items_vbars :: { () }
    : alg_data_type_items_vbars impltype vbars  { () }
    | vbars                                     { () }
    | {- empty -} %shift                        { () }

vbars :: { () }
    : vbars '|'     { () }
    | '|'           { () }


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


type :: { () }
    : '\\/' bind_vars '=>' type     { () }
    | type_unit '->' type           { () }
    | type_unit %shift              { () }

type_unit :: { () }
    : type_infix %shift         { () }

type_infix :: { () }
    : type_infix type_op type_apps %shift   { () }
    | type_apps %shift                      { () }

type_op :: { () }
    : CONSYM                        { () }
    | var_sym_ext                   { () }
    | '`' type_qualified_op '`'     { () }

type_qualified_op :: { () }
    : sym_ext       { () }
    | type_block    { () }

type_apps :: { () }
    : type_apps type_app        { () }
    | type_qualified            { () }

type_app :: { () }
    : '@' type_qualified    { () }
    | type_qualified        { () }

type_qualified :: { () }
    : type_block                  { () }

type_block :: { () }
    : type_atomic                       { () }

type_atomic :: { () }
    : '(' type may_type_sig ')'         { () }
    | con                               { () }
    | var                               { () }
    | literal                           { () }
    | '(' type_tuple_items ')'          { () }
    | '[' type_array_items ']'          { () }
    | '{' type_simplrecord_items '}'    { () }

type_tuple_items :: { () }
    : type_tuple_items_commas type ','   { () }
    | type_tuple_items_commas type       { () }

type_tuple_items_commas :: { () }
    : type_tuple_items_commas type ','  { () }
    | type ','                          { () }

type_array_items :: { () }
    : type_array_items_commas type  { () }
    | type_array_items_commas       { () }

type_array_items_commas :: { () }
    : type_array_items_commas type ','  { () }
    | {- empty -}                       { () }

type_simplrecord_items :: { () }
    : type_simplrecord_items_commas type_simplrecord_item   { () }
    | type_simplrecord_items_commas                         { () }

type_simplrecord_items_commas :: { () }
    : type_simplrecord_items_commas type_simplrecord_item ','   { () }
    | {- empty -}                                               { () }

type_simplrecord_item :: { () }
    : var ':' type      { () }


sig_item :: { Ast.Decl C }
    : typesig_decl      { undefined }
    | valsig_decl       { undefined }


expr :: { () }
    : expr_unit may_type_sig    { () }

expr_unit :: { () }
    : expr_infix %shift         { $1 }

expr_infix :: { () }
    : expr_infix expr_op expr_apps %shift   { () }
    | expr_apps %shift                      { () }

expr_op :: { () }
    : CONSYM                        { () }
    | var_sym_ext                   { () }
    | '`' expr_qualified_op '`'     { () }

expr_qualified_op :: { () }
    : sym_ext           { () }
    | expr_block        { () }

expr_apps :: { () }
    : expr_apps expr_app        { () }
    | expr_qualified            { () }

expr_app :: { () }
    : '@' expr_qualified        { () }
    | expr_qualified            { () }

expr_qualified :: { () }
    : expr_block                  { () }

expr_block :: { () }
    : '\\' 'case' case_alt_body             { () }
    | '\\' 'when' guarded_alt_body          { () }
    | '\\' lambda_body                      { () } -- conflict with expr
    | 'let' let_body                        { () } -- conflict with expr
    | 'letrec' let_body                     { () } -- conflict with expr
    | 'case' case_body                      { () }
    | 'do' do_body                          { () }
    | expr_atomic                           { () }

expr_atomic :: { () }
    : '(' expr ')'                  { () }
    | con                           { () }
    | var                           { () }
    | expr_literal                  { () }

expr_literal :: { () }
    : literal                           { () }
    | expr_interp_string                { () }
    | '(' expr_tuple_items ')'          { () }
    | '[' expr_array_items ']'          { () }
    | '{' expr_simplrecord_items '}'    { () }

expr_interp_string :: { () }
    : INTERP_STRING_WITHOUT_INTERP                                          { () }
    | INTERP_STRING_START expr expr_interp_string_conts INTERP_STRING_END   { () }

expr_interp_string_conts :: { () }
    : expr_interp_string_conts INTERP_STRING_CONTINUE expr  { () }
    | {- empty -}                                           { () }

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
    : pat_unit may_type_sig     { () }

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
    : sym_ext    { () }
    | pat_qualified                 { () }

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
    : do_stmt_items_semis expr                  { () }
    | do_stmt_items_semis do_stmt_item lsemis   { () } -- do_stmt_items_semis expr lsemis

do_stmt_items_semis :: { () }
    : do_stmt_items_semis do_stmt_item lsemis   { () }
    | {- empty -}                               { () }

do_stmt_item :: { () }
    : expr                          { () }
    | expr '<-' expr                { () } -- pat '<-' expr
    | expr '=' expr                 { () } -- pat '=' expr
    | 'rec' let_binds               { () }


bind_var :: { () }
    : '@' simple_bind_var           { () }
    | simple_bind_var               { () }

simple_bind_var :: { (S (Ast.BindVar C)) }
    : var_id_ext                    { undefined }
    | '(' var_id_ext ':' type ')'   { undefined }

con :: { S Ast.Name }
    : con_id_ext            { $1 }
    | '(' con_sym_ext ')'   { spn ($1, $2, $3) do unS $2 }

conop :: { S Ast.Name }
    : con_sym_ext           { $1 }
    | '`' con_sym_ext '`'   { spn ($1, $2, $3) do unS $2 }
    | '`' con_id_ext '`'    { spn ($1, $2, $3) do unS $2 }

var :: { S Ast.Name }
    : var_id_ext                { $1 }
    | '(' var_sym_ext ')'       { spn ($1, $2, $3) do unS $2 }

sym_ext :: { S Ast.Name }
    : con_sym_ext       { $1 }
    | var_sym_ext       { $1 }


declcon :: { S Ast.Name }
    : conid             { $1 }
    | '(' consym ')'    { spn ($1, $2, $3) do unS $2 }

declconop :: { S Ast.Name }
    : consym            { $1 }
    | '`' conid '`'     { spn ($1, $2, $3) do unS $2 }

con_id_ext :: { S Ast.Name }
    : conid             { $1 }
    | '(' ')'           { spn $1 do mkName "()" }

con_sym_ext :: { S Ast.Name }
    : consym            { $1 }
    | '->'              { spn $1 do mkName "->" }

declvar :: { S Ast.Name }
    : varid             { $1 }
    | '`' varsym '`'    { spn ($1, $2, $3) do unS $2 }

declop :: { S Ast.Name }
    : varsym            { $1 }
    | '`' varid '`'     { spn ($1, $2, $3) do unS $2 }

var_id_ext :: { S Ast.Name }
    : varid     { $1 }
    | '_'       { spn $1 do mkName "_" }

var_sym_ext :: { S Ast.Name }
    : varsym    { $1 }


lopen :: { () }
    : lopen VSEMI   { () }
    | '{'           { () }
    | '{{'          { () }
    | VOBRACE       { () }

lclose :: { () }
    : '}'           { () }
    | '}}'          { () }
    | VCBRACE       { () }
    | error         { () }

lsemis :: { () }
    : lsemis semi   { () }
    | semi          { () }

semi :: { () }
    : ';'       { () }
    | VSEMI     { () }


literal :: { () }
    : BYTECHAR              { () }
    | BYTESTRING            { () }
    | CHAR                  { () }
    | STRING                { () }
    | INTEGER               { () }
    | RATIONAL              { () }

may_type_sig :: { () }
    : ':' type              { () }
    | {- empty -} %shift    { () }

bind_vars :: { () }
    : bind_vars bind_var    { () }
    | {- empty -}           { () }


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
type C = AstParsed
data AstParsed

type S = Spanned.T

pattern S :: a -> S a
pattern S x <- Spanned.Spanned
    {
        getSpan = _,
        unSpanned = x
    }

unS :: S a -> a
unS sx = Spanned.unSpanned sx

class SpannedBuilder s where
    sp :: s -> Spanned.Span

    spn :: s -> a -> S a
    spn s x = Spanned.Spanned
        {
            getSpan = sp s,
            unSpanned = x
        }

instance SpannedBuilder Spanned.Span where
    sp s = s

instance SpannedBuilder (S a) where
    sp sx = Spanned.getSpan sx

instance (SpannedBuilder s1, SpannedBuilder s2) => SpannedBuilder (s1, s2) where
    sp (s1, s2) = sp s1 <> sp s2

instance (SpannedBuilder s1, SpannedBuilder s2, SpannedBuilder s3)
        => SpannedBuilder (s1, s2, s3) where
    sp (s1, s2, s3) = sp s1 <> sp s2 <> sp s3

instance (SpannedBuilder s1, SpannedBuilder s2, SpannedBuilder s3, SpannedBuilder s4)
        => SpannedBuilder (s1, s2, s3, s4) where
    sp (s1, s2, s3, s4) = sp s1 <> sp s2 <> sp s3 <> sp s4

mkName :: StringLit -> Ast.Name
mkName s = Ast.mkName do text s

lexer = undefined

happyError = undefined
}
