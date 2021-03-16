module Language.Quell.Type.Ast (
    Program (..),

    Decl (..),
    XDeclTypeSig,
    XDeclValSig,
    XDeclConSig,
    XDeclType,
    XDeclDataType,
    XDeclVal,
    XDeclValBind,
    XDeclMonBind,

    TypeSigDecl (..),
    XTypeSigDecl,

    ValSigDecl (..),
    XValSigDecl,

    ConSigDecl (..),
    XConSigDecl,

    TypeDecl (..),
    XTypeDecl,

    DataTypeDecl (..),
    ValDecl (..),

    ValBind (..),
    XValBind,

    MonBind (..),
    XMonBind,

    DeclType (..),
    XDeclAppType,
    XDeclInfixType,

    ImplType (..),
    XImplAppType,
    XImplInfixType,

    DeclExpr (..),

    TypeExpr (..),
    XTypeForall,
    XTypeInfix,
    XTypeApp,
    XTypeSig,
    XTypeCon,
    XTypeVar,
    XTypeLit,
    XTypeTuple,
    XTypeArray,
    XTypeRecord,
    XTypeAnn,

    Expr (..),
    XExprSig,
    XExprInfix,
    XExprApp,
    XExprLambda,
    XExprCase,
    XExprDo,
    XExprLetrec,
    XExprLet,
    XExprCon,
    XExprVar,
    XExprLit,
    XExprInterpString,
    XExprTuple,
    XExprArray,
    XExprRecord,
    XExprAnn,

    InterpStringPart (..),
    XInterpStringLit,
    XInterpStringExpr,

    AppType (..),
    XAppType,
    XUnivAppType,

    AppExpr (..),
    XAppExpr,
    XUnivAppExpr,

    Lit (..),
    XLitRational,
    XLitInteger,
    XLitByteString,
    XLitString,
    XLitByteChar,
    XLitChar,

    BindVar (..),
    XBindVar,
    XUnivBindVar,

    Name,
    mkName,
    primNameUnit,
    primNameArrow,
    primNameWildcard,

    XEq,
    XShow,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Type.TextId as TextId


data Program c = Program
    {
        decls      :: [Decl c]
    }

deriving instance XEq c => Eq (Program c)
deriving instance XShow c => Show (Program c)


data Decl c
    = DeclTypeSig (TypeSigDecl c) (XDeclTypeSig c)
    | DeclValSig (ValSigDecl c) (XDeclValSig c)
    | DeclConSig (ConSigDecl c) (XDeclConSig c)
    | DeclType (TypeDecl c) (XDeclType c)
    | DeclDataType (DataTypeDecl c) (XDeclDataType c)
    | DeclVal (ValDecl c) (XDeclVal c)
    | DeclValBind (ValBind c) (XDeclValBind c)
    | DeclMonBind (MonBind c) (XDeclMonBind c)

type family XDeclTypeSig c :: Type
type family XDeclValSig c :: Type
type family XDeclConSig c :: Type
type family XDeclType c :: Type
type family XDeclDataType c :: Type
type family XDeclVal c :: Type
type family XDeclValBind c :: Type
type family XDeclMonBind c :: Type

deriving instance XEq c => Eq (Decl c)
deriving instance XShow c => Show (Decl c)


data TypeSigDecl c = TypeSigDecl Name (TypeExpr c) (XTypeSigDecl c)

type family XTypeSigDecl c :: Type

deriving instance XEq c => Eq (TypeSigDecl c)
deriving instance XShow c => Show (TypeSigDecl c)


data ValSigDecl c = ValSigDecl Name (TypeExpr c) (XValSigDecl c)

type family XValSigDecl c :: Type

deriving instance XEq c => Eq (ValSigDecl c)
deriving instance XShow c => Show (ValSigDecl c)


data ConSigDecl c = ConSigDecl Name (TypeExpr c) (XConSigDecl c)

type family XConSigDecl c :: Type

deriving instance XEq c => Eq (ConSigDecl c)
deriving instance XShow c => Show (ConSigDecl c)


data TypeDecl c = TypeDecl (DeclType c) (TypeExpr c) [Decl c] (XTypeDecl c)

type family XTypeDecl c :: Type

deriving instance XEq c => Eq (TypeDecl c)
deriving instance XShow c => Show (TypeDecl c)


data DataTypeDecl c
    = DataTypeDecl
        Name
        -- ^ con
        (Maybe (TypeExpr c))
        -- ^ type sig
        [Decl c]
        -- ^ body
    | AlgDataTypeDecl
        (DeclType c)
        -- ^ decl type
        [ImplType c]
        -- ^ body
        [Decl c]
        -- ^ assumptions

deriving instance XEq c => Eq (DataTypeDecl c)
deriving instance XShow c => Show (DataTypeDecl c)


data ValDecl c = ValDecl
    {
        valDeclVar :: DeclExpr c,
        valDeclExpr :: Expr c,
        valDeclAssumptions :: [Decl c]
    }

deriving instance XEq c => Eq (ValDecl c)
deriving instance XShow c => Show (ValDecl c)


data ValBind c = ValBind (Pat c) (Expr c) [Decl c] (XValBind c)

type family XValBind c :: Type

deriving instance XEq c => Eq (ValBind c)
deriving instance XShow c => Show (ValBind c)


data MonBind c = MonBind (Pat c) (Expr c) [Decl c] (XMonBind c)

type family XMonBind c :: Type

deriving instance XEq c => Eq (MonBind c)
deriving instance XShow c => Show (MonBind c)


data DeclType c
    = DeclAppType Name [BindVar c] (XDeclAppType c)
    | DeclInfixType (BindVar c) Name (BindVar c) (XDeclInfixType c)

type family XDeclAppType c :: Type
type family XDeclInfixType c :: Type

deriving instance XEq c => Eq (DeclType c)
deriving instance XShow c => Show (DeclType c)


data ImplType c
    = ImplAppType Name [AppType c] (XImplAppType c)
    | ImplInfixType (TypeExpr c) Name (TypeExpr c) (XImplInfixType c)

type family XImplAppType c :: Type
type family XImplInfixType c :: Type

deriving instance XEq c => Eq (ImplType c)
deriving instance XShow c => Show (ImplType c)


data DeclExpr c
    = DeclAppExpr Name [BindVar c]
    | DeclInfixExpr (BindVar c) Name (BindVar c)

deriving instance XEq c => Eq (DeclExpr c)
deriving instance XShow c => Show (DeclExpr c)


data TypeExpr c
    = TypeForall [BindVar c] (TypeExpr c) (XTypeForall c)
    | TypeInfix (TypeExpr c) (TypeExpr c) (TypeExpr c) (XTypeInfix c)
    | TypeApp (TypeExpr c) [AppType c] (XTypeApp c)
    | TypeSig (TypeExpr c) (TypeExpr c) (XTypeSig c)
    | TypeCon Name (XTypeCon c)
    | TypeVar Name (XTypeVar c)
    | TypeLit (Lit c) (XTypeLit c)
    | TypeTuple [TypeExpr c] (XTypeTuple c)
    | TypeArray [TypeExpr c] (XTypeArray c)
    | TypeRecord [(Name, TypeExpr c)] (XTypeRecord c)
    | TypeAnn (TypeExpr c) (XTypeAnn c)

type family XTypeForall c :: Type
type family XTypeInfix c :: Type
type family XTypeApp c :: Type
type family XTypeSig c :: Type
type family XTypeCon c :: Type
type family XTypeVar c :: Type
type family XTypeLit c :: Type
type family XTypeTuple c :: Type
type family XTypeArray c :: Type
type family XTypeRecord c :: Type
type family XTypeAnn c :: Type

deriving instance XEq c => Eq (TypeExpr c)
deriving instance XShow c => Show (TypeExpr c)


data AppType c
    = AppType (TypeExpr c) (XAppType c)
    | UnivAppType (TypeExpr c) (XUnivAppType c)

type family XAppType c :: Type
type family XUnivAppType c :: Type

deriving instance XEq c => Eq (AppType c)
deriving instance XShow c => Show (AppType c)


data Expr c
    = ExprSig (Expr c) (TypeExpr c) (XExprSig c)
    | ExprInfix (Expr c) (Expr c) (Expr c) (XExprInfix c)
    | ExprApp (Expr c) [AppExpr c] (XExprApp c)
    | ExprLambda [CaseAlt c] (XExprLambda c)
    | ExprLetrec [Decl c] (Expr c) (XExprLetrec c)
    | ExprLet [Decl c] (Expr c) (XExprLet c)
    | ExprCase [Expr c] [CaseAlt c] (XExprCase c)
    | ExprDo [DoStmt c] (Expr c) (XExprDo c)
    | ExprCon Name (XExprCon c)
    | ExprVar Name (XExprVar c)
    | ExprLit (Lit c) (XExprLit c)
    | ExprInterpString [InterpStringPart c] (XExprInterpString c)
    | ExprTuple [Expr c] (XExprTuple c)
    | ExprArray [Expr c] (XExprArray c)
    | ExprRecord [(Name, Expr c)] (XExprRecord c)
    | ExprAnn (Expr c) (XExprAnn c)

type family XExprSig c :: Type
type family XExprInfix c :: Type
type family XExprApp c :: Type
type family XExprLambda c :: Type
type family XExprLetrec c :: Type
type family XExprLet c :: Type
type family XExprCase c :: Type
type family XExprDo c :: Type
type family XExprCon c :: Type
type family XExprVar c :: Type
type family XExprLit c :: Type
type family XExprInterpString c :: Type
type family XExprTuple c :: Type
type family XExprArray c :: Type
type family XExprRecord c :: Type
type family XExprAnn c :: Type

deriving instance XEq c => Eq (Expr c)
deriving instance XShow c => Show (Expr c)


data AppExpr c
    = AppExpr (Expr c) (XAppExpr c)
    | UnivAppExpr (TypeExpr c) (XUnivAppExpr c)

type family XAppExpr c :: Type
type family XUnivAppExpr c :: Type

deriving instance XEq c => Eq (AppExpr c)
deriving instance XShow c => Show (AppExpr c)


data CaseAlt c = CaseAlt
    {
        caseAltPats :: [Pat c],
        caseAltGuardedAlts :: [GuardedAlt c]
    }

deriving instance XEq c => Eq (CaseAlt c)
deriving instance XShow c => Show (CaseAlt c)


data GuardedAlt c = GuardedAlt
    {
        guardedAltGuard :: Maybe (Expr c),
        guardedAltExpr :: Expr c
    }

deriving instance XEq c => Eq (GuardedAlt c)
deriving instance XShow c => Show (GuardedAlt c)


data DoStmt c
    = DoStmtExpr (Expr c)
    | DoStmtBind (Pat c) (Expr c)
    | DoStmtLet [Decl c]
    | DoStmtLetrec [Decl c]

deriving instance XEq c => Eq (DoStmt c)
deriving instance XShow c => Show (DoStmt c)


data Pat c
    = PatSig (Pat c) (TypeExpr c)
    | PatOr [Pat c]
    | PatInfix (Pat c) Name (Pat c)
    | PatApp (Pat c) [AppPat c]
    | PatCon Name
    | PatVar Name
    | PatWildcard Name
    | PatLit (Lit c)
    | PatTuple [Pat c]
    | PatArray [Pat c]
    | PatRecord [(Name, Pat c)]

deriving instance XEq c => Eq (Pat c)
deriving instance XShow c => Show (Pat c)


data AppPat c
    = AppPat (Pat c)
    | UnivAppPat (TypeExpr c)

deriving instance XEq c => Eq (AppPat c)
deriving instance XShow c => Show (AppPat c)


data Lit c
    = LitRational Rational (XLitRational c)
    | LitInteger Integer (XLitInteger c)
    | LitByteString ByteString (XLitByteString c)
    | LitString Text (XLitString c)
    | LitByteChar Word8 (XLitByteChar c)
    | LitChar Char (XLitChar c)

type family XLitRational c :: Type
type family XLitInteger c :: Type
type family XLitByteString c :: Type
type family XLitString c :: Type
type family XLitByteChar c :: Type
type family XLitChar c :: Type

deriving instance XEq c => Eq (Lit c)
deriving instance XShow c => Show (Lit c)


data InterpStringPart c
    = InterpStringLit Text (XInterpStringLit c)
    | InterpStringExpr (Expr c) (XInterpStringExpr c)

type family XInterpStringLit c :: Type
type family XInterpStringExpr c :: Type

deriving instance XEq c => Eq (InterpStringPart c)
deriving instance XShow c => Show (InterpStringPart c)


data BindVar c
    = BindVar Name (Maybe (TypeExpr c)) (XBindVar c)
    | UnivBindVar Name (Maybe (TypeExpr c)) (XUnivBindVar c)

type family XBindVar c :: Type
type family XUnivBindVar c :: Type

deriving instance XEq c => Eq (BindVar c)
deriving instance XShow c => Show (BindVar c)


type Name = TextId.T

mkName :: Text -> Name
mkName n = TextId.textId n

primNameUnit :: Name
primNameUnit = TextId.primTextId TextId.PrimTextUnit

primNameArrow :: Name
primNameArrow = TextId.primTextId TextId.PrimTextArrow

primNameWildcard :: Name
primNameWildcard = TextId.primTextId TextId.PrimTextWildcard


type XC :: (Type -> Constraint) -> Type -> Constraint
type XC f c =
    (
        f (XAppExpr c),
        f (XUnivAppExpr c),
        f (XBindVar c),
        f (XUnivBindVar c),
        f (XLitRational c),
        f (XLitInteger c),
        f (XLitByteString c),
        f (XLitString c),
        f (XLitByteChar c),
        f (XLitChar c),
        f (XTypeForall c),
        f (XTypeInfix c),
        f (XTypeApp c),
        f (XTypeSig c),
        f (XTypeCon c),
        f (XTypeVar c),
        f (XTypeLit c),
        f (XTypeTuple c),
        f (XTypeArray c),
        f (XTypeRecord c),
        f (XTypeAnn c),
        f (XAppType c),
        f (XUnivAppType c),
        f (XTypeSigDecl c),
        f (XValSigDecl c),
        f (XConSigDecl c),
        f (XTypeDecl c),
        f (XValBind c),
        f (XMonBind c),
        f (XDeclTypeSig c),
        f (XDeclValSig c),
        f (XDeclConSig c),
        f (XDeclType c),
        f (XDeclDataType c),
        f (XDeclVal c),
        f (XDeclValBind c),
        f (XDeclMonBind c),
        f (XExprSig c),
        f (XExprInfix c),
        f (XExprLambda c),
        f (XExprApp c),
        f (XExprLetrec c),
        f (XExprLet c),
        f (XExprCase c),
        f (XExprDo c),
        f (XExprCon c),
        f (XExprVar c),
        f (XExprLit c),
        f (XExprInterpString c),
        f (XExprTuple c),
        f (XExprArray c),
        f (XExprRecord c),
        f (XExprAnn c),
        f (XInterpStringLit c),
        f (XInterpStringExpr c),
        f (XDeclAppType c),
        f (XDeclInfixType c),
        f (XImplAppType c),
        f (XImplInfixType c)
    )

class XC Eq c => XEq c
class XC Show c => XShow c
