module Language.Quell.Type.Ast (
    Program (..),
    XProgram,

    Decl (..),
    XDeclTypeSig,
    XDeclValSig,
    XDeclConSig,
    XDeclType,
    XDeclDataType,
    XDeclAlgDataType,
    XDeclNewType,
    XDeclVal,
    XDeclValBind,

    DeclType (..),
    XDeclAppType,
    XDeclInfixType,

    ImplType (..),
    XImplAppType,
    XImplInfixType,

    DeclExpr (..),
    XDeclAppExpr,
    XDeclInfixExpr,

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
    XExprMatch,
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

    TypeRecordItem (..),
    XTypeRecordItem,

    AppExpr (..),
    XAppExpr,
    XUnivAppExpr,

    ExprRecordItem (..),
    XExprRecordItem,

    CaseAlt (..),
    XCaseAlt,

    GuardedAlt (..),
    XGuardedAlt,

    DoStmt (..),
    XDoStmtMonBind,
    XDoStmtBind,
    XDoStmtLetrec,

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

    Pat (..),
    XPatSig,
    XPatOr,
    XPatInfix,
    XPatApp,
    XPatUnivApp,
    XPatVar,
    XPatCon,
    XPatLit,
    XPatWildcard,
    XPatTuple,
    XPatArray,
    XPatRecord,
    XPatAnn,

    PatOp (..),
    XPatOpConApp,

    AppPat (..),
    XAppPat,
    XUnivAppPat,

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


data Program c = Program [Decl c] (XProgram c)

type family XProgram c :: Type

type XCProgram f c =
    (
        f (XProgram c)
    )

deriving instance XEq c => Eq (Program c)
deriving instance XShow c => Show (Program c)


data Decl c
    = DeclTypeSig Name (TypeExpr c) (XDeclTypeSig c)
    | DeclValSig Name (TypeExpr c) (XDeclValSig c)
    | DeclConSig Name (TypeExpr c) (XDeclConSig c)
    | DeclType (DeclType c) (TypeExpr c) [Decl c] (XDeclType c)
    | DeclDataType Name (Maybe (TypeExpr c)) [Decl c] (XDeclDataType c)
    | DeclAlgDataType (DeclType c) [ImplType c] [Decl c] (XDeclAlgDataType c)
    | DeclNewType (DeclType c) (TypeExpr c) [Decl c] (XDeclNewType c)
    | DeclVal (DeclExpr c) (Expr c) [Decl c] (XDeclVal c)
    | DeclValBind (Pat c) (Expr c) [Decl c] (XDeclValBind c)

type family XDeclTypeSig c :: Type
type family XDeclValSig c :: Type
type family XDeclConSig c :: Type
type family XDeclType c :: Type
type family XDeclDataType c :: Type
type family XDeclAlgDataType c :: Type
type family XDeclNewType c :: Type
type family XDeclVal c :: Type
type family XDeclValBind c :: Type

type XCDecl :: (Type -> Constraint) -> Type -> Constraint
type XCDecl f c =
    (
        f (XDeclTypeSig c),
        f (XDeclValSig c),
        f (XDeclConSig c),
        f (XDeclType c),
        f (XDeclDataType c),
        f (XDeclAlgDataType c),
        f (XDeclNewType c),
        f (XDeclVal c),
        f (XDeclValBind c)
    )

deriving instance XEq c => Eq (Decl c)
deriving instance XShow c => Show (Decl c)


data DeclType c
    = DeclAppType Name [BindVar c] (Maybe (TypeExpr c)) (XDeclAppType c)
    | DeclInfixType (BindVar c) Name (BindVar c) (Maybe (TypeExpr c)) (XDeclInfixType c)

type family XDeclAppType c :: Type
type family XDeclInfixType c :: Type

type XCDeclType :: (Type -> Constraint) -> Type -> Constraint
type XCDeclType f c =
    (
        f (XDeclAppType c),
        f (XDeclInfixType c)
    )

deriving instance XEq c => Eq (DeclType c)
deriving instance XShow c => Show (DeclType c)


data ImplType c
    = ImplAppType Name [AppType c] (XImplAppType c)
    | ImplInfixType (TypeExpr c) Name (TypeExpr c) (XImplInfixType c)

type family XImplAppType c :: Type
type family XImplInfixType c :: Type

type XCImplType :: (Type -> Constraint) -> Type -> Constraint
type XCImplType f c =
    (
        f (XImplAppType c),
        f (XImplInfixType c)
    )

deriving instance XEq c => Eq (ImplType c)
deriving instance XShow c => Show (ImplType c)


data DeclExpr c
    = DeclAppExpr Name [BindVar c] (Maybe (TypeExpr c)) (XDeclAppExpr c)
    | DeclInfixExpr (BindVar c) Name (BindVar c) (Maybe (TypeExpr c)) (XDeclInfixExpr c)

type family XDeclAppExpr c :: Type
type family XDeclInfixExpr c :: Type

type XCDeclExpr :: (Type -> Constraint) -> Type -> Constraint
type XCDeclExpr f c =
    (
        f (XDeclAppExpr c),
        f (XDeclInfixExpr c)
    )

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
    | TypeRecord [TypeRecordItem c] (XTypeRecord c)
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

type XCTypeExpr :: (Type -> Constraint) -> Type -> Constraint
type XCTypeExpr f c =
    (
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
        f (XTypeAnn c)
    )

deriving instance XEq c => Eq (TypeExpr c)
deriving instance XShow c => Show (TypeExpr c)


data AppType c
    = AppType (TypeExpr c) (XAppType c)
    | UnivAppType (TypeExpr c) (XUnivAppType c)

type family XAppType c :: Type
type family XUnivAppType c :: Type

type XCAppType :: (Type -> Constraint) -> Type -> Constraint
type XCAppType f c =
    (
        f (XAppType c),
        f (XUnivAppType c)
    )

deriving instance XEq c => Eq (AppType c)
deriving instance XShow c => Show (AppType c)


data TypeRecordItem c = TypeRecordItem Name (TypeExpr c) (XTypeRecordItem c)

type family XTypeRecordItem c :: Type

type XCTypeRecordItem :: (Type -> Constraint) -> Type -> Constraint
type XCTypeRecordItem f c =
    (
        f (XTypeRecordItem c)
    )

deriving instance XEq c => Eq (TypeRecordItem c)
deriving instance XShow c => Show (TypeRecordItem c)


data Expr c
    = ExprSig (Expr c) (TypeExpr c) (XExprSig c)
    | ExprInfix (Expr c) (Expr c) (Expr c) (XExprInfix c)
    | ExprApp (Expr c) [AppExpr c] (XExprApp c)
    | ExprLambda [CaseAlt c] (XExprLambda c)
    | ExprLetrec [Decl c] (Expr c) (XExprLetrec c)
    | ExprLet [Decl c] (Expr c) (XExprLet c)
    | ExprMatch [Expr c] [CaseAlt c] (XExprMatch c)
    | ExprDo [DoStmt c] (Expr c) (XExprDo c)
    | ExprCon Name (XExprCon c)
    | ExprVar Name (XExprVar c)
    | ExprLit (Lit c) (XExprLit c)
    | ExprInterpString (NonEmpty (InterpStringPart c)) (XExprInterpString c)
    | ExprTuple [Expr c] (XExprTuple c)
    | ExprArray [Expr c] (XExprArray c)
    | ExprRecord [ExprRecordItem c] (XExprRecord c)
    | ExprAnn (Expr c) (XExprAnn c)

type family XExprSig c :: Type
type family XExprInfix c :: Type
type family XExprApp c :: Type
type family XExprLambda c :: Type
type family XExprLetrec c :: Type
type family XExprLet c :: Type
type family XExprMatch c :: Type
type family XExprDo c :: Type
type family XExprCon c :: Type
type family XExprVar c :: Type
type family XExprLit c :: Type
type family XExprInterpString c :: Type
type family XExprTuple c :: Type
type family XExprArray c :: Type
type family XExprRecord c :: Type
type family XExprAnn c :: Type

type XCExpr :: (Type -> Constraint) -> Type -> Constraint
type XCExpr f c =
    (
        f (XExprSig c),
        f (XExprInfix c),
        f (XExprApp c),
        f (XExprLambda c),
        f (XExprLetrec c),
        f (XExprLet c),
        f (XExprMatch c),
        f (XExprDo c),
        f (XExprCon c),
        f (XExprVar c),
        f (XExprLit c),
        f (XExprInterpString c),
        f (XExprTuple c),
        f (XExprArray c),
        f (XExprRecord c),
        f (XExprAnn c)
    )

deriving instance XEq c => Eq (Expr c)
deriving instance XShow c => Show (Expr c)


data AppExpr c
    = AppExpr (Expr c) (XAppExpr c)
    | UnivAppExpr (TypeExpr c) (XUnivAppExpr c)

type family XAppExpr c :: Type
type family XUnivAppExpr c :: Type

type XCAppExpr :: (Type -> Constraint) -> Type -> Constraint
type XCAppExpr f c =
    (
        f (XAppExpr c),
        f (XUnivAppExpr c)
    )

deriving instance XEq c => Eq (AppExpr c)
deriving instance XShow c => Show (AppExpr c)


data ExprRecordItem c = ExprRecordItem Name (Expr c) (XExprRecordItem c)

type family XExprRecordItem c :: Type

type XCExprRecordItem :: (Type -> Constraint) -> Type -> Constraint
type XCExprRecordItem f c =
    (
        f (XExprRecordItem c)
    )

deriving instance XEq c => Eq (ExprRecordItem c)
deriving instance XShow c => Show (ExprRecordItem c)


data CaseAlt c = CaseAlt [Pat c] [GuardedAlt c] (XCaseAlt c)

type family XCaseAlt c :: Type

type XCCaseAlt f c =
    (
        f (XCaseAlt c)
    )

deriving instance XEq c => Eq (CaseAlt c)
deriving instance XShow c => Show (CaseAlt c)


data GuardedAlt c = GuardedAlt (Maybe (Expr c)) (Expr c) (XGuardedAlt c)

type family XGuardedAlt c :: Type

type XCGuardedAlt f c =
    (
        f (XGuardedAlt c)
    )

deriving instance XEq c => Eq (GuardedAlt c)
deriving instance XShow c => Show (GuardedAlt c)


data DoStmt c
    = DoStmtBind (Pat c) (Expr c) [Decl c] (XDoStmtBind c)
    | DoStmtMonBind (Pat c) (Expr c) [Decl c] (XDoStmtMonBind c)
    | DoStmtLetrec [Decl c] (XDoStmtLetrec c)

type family XDoStmtBind c :: Type
type family XDoStmtMonBind c :: Type
type family XDoStmtLetrec c :: Type

type XCDoStmt :: (Type -> Constraint) -> Type -> Constraint
type XCDoStmt f c =
    (
        f (XDoStmtBind c),
        f (XDoStmtMonBind c),
        f (XDoStmtLetrec c)
    )

deriving instance XEq c => Eq (DoStmt c)
deriving instance XShow c => Show (DoStmt c)


data Pat c
    = PatSig (Pat c) (TypeExpr c) (XPatSig c)
    | PatOr [Pat c] (XPatOr c)
    | PatInfix (Pat c) (PatOp c) (Pat c) (XPatInfix c)
    | PatConApp Name [AppPat c] (XPatApp c)
    | PatUnivApp (Pat c) [TypeExpr c] (XPatUnivApp c)
    | PatVar Name (XPatVar c)
    | PatWildcard Name (XPatWildcard c)
    | PatLit (Lit c) (XPatLit c)
    | PatTuple [Pat c] (XPatTuple c)
    | PatArray [Pat c] (XPatArray c)
    | PatRecord [(Name, Pat c)] (XPatRecord c)
    | PatAnn (Pat c) (XPatAnn c)

type family XPatSig c :: Type
type family XPatOr c :: Type
type family XPatInfix c :: Type
type family XPatApp c :: Type
type family XPatUnivApp c :: Type
type family XPatCon c :: Type
type family XPatVar c :: Type
type family XPatWildcard c :: Type
type family XPatLit c :: Type
type family XPatTuple c :: Type
type family XPatArray c :: Type
type family XPatRecord c :: Type
type family XPatAnn c :: Type

type XCPat :: (Type -> Constraint) -> Type -> Constraint
type XCPat f c =
    (
        f (XPatSig c),
        f (XPatOr c),
        f (XPatInfix c),
        f (XPatApp c),
        f (XPatUnivApp c),
        f (XPatCon c),
        f (XPatVar c),
        f (XPatWildcard c),
        f (XPatLit c),
        f (XPatTuple c),
        f (XPatArray c),
        f (XPatRecord c),
        f (XPatAnn c)
    )

deriving instance XEq c => Eq (Pat c)
deriving instance XShow c => Show (Pat c)


data PatOp c
    = PatOpConApp Name [AppPat c] (XPatOpConApp c)

type family XPatOpConApp c :: Type

type XCPatOp :: (Type -> Constraint) -> Type -> Constraint
type XCPatOp f c = f (XPatOpConApp c)

deriving instance XEq c => Eq (PatOp c)
deriving instance XShow c => Show (PatOp c)


data AppPat c
    = AppPat (Pat c) (XAppPat c)
    | UnivAppPat (TypeExpr c) (XUnivAppPat c)

type family XAppPat c :: Type
type family XUnivAppPat c :: Type

type XCAppPat :: (Type -> Constraint) -> Type -> Constraint
type XCAppPat f c =
    (
        f (XAppPat c),
        f (XUnivAppPat c)
    )

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

type XCLit :: (Type -> Constraint) -> Type -> Constraint
type XCLit f c =
    (
        f (XLitRational c),
        f (XLitInteger c),
        f (XLitByteString c),
        f (XLitString c),
        f (XLitByteChar c),
        f (XLitChar c)
    )

deriving instance XEq c => Eq (Lit c)
deriving instance XShow c => Show (Lit c)


data InterpStringPart c
    = InterpStringLit Text (XInterpStringLit c)
    | InterpStringExpr (Expr c) (XInterpStringExpr c)

type family XInterpStringLit c :: Type
type family XInterpStringExpr c :: Type

type XCInterpStringPart :: (Type -> Constraint) -> Type -> Constraint
type XCInterpStringPart f c =
    (
        f (XInterpStringLit c),
        f (XInterpStringExpr c)
    )

deriving instance XEq c => Eq (InterpStringPart c)
deriving instance XShow c => Show (InterpStringPart c)


data BindVar c
    = BindVar Name (Maybe (TypeExpr c)) (XBindVar c)
    | UnivBindVar Name (Maybe (TypeExpr c)) (XUnivBindVar c)

type family XBindVar c :: Type
type family XUnivBindVar c :: Type

type XCBindVar :: (Type -> Constraint) -> Type -> Constraint
type XCBindVar f c =
    (
        f (XBindVar c),
        f (XUnivBindVar c)
    )

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
        XCProgram f c,
        XCAppExpr f c,
        XCBindVar f c,
        XCLit f c,
        XCTypeExpr f c,
        XCDecl f c,
        XCExpr f c,
        XCInterpStringPart f c,
        XCExprRecordItem f c,
        XCDeclType f c,
        XCImplType f c,
        XCDoStmt f c,
        XCPat f c,
        XCPatOp f c,
        XCAppType f c,
        XCTypeRecordItem f c,
        XCAppPat f c,
        XCDeclExpr f c,
        XCGuardedAlt f c,
        XCCaseAlt f c
    )

class XC Eq c => XEq c
class XC Show c => XShow c
