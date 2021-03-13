module Language.Quell.Type.Ast (
  Program (..),
  Decl (..),
  TypeSigDecl (..),
  ValSigDecl (..),
  ConSigDecl (..),
  TypeDecl (..),
  DataTypeDecl (..),
  ValDecl (..),
  TypeExpr (..),

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
  XLitInterpString,

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
    = DeclTypeSig (TypeSigDecl c)
    | DeclValSig (ValSigDecl c)
    | DeclConSig (ConSigDecl c)
    | DeclType (TypeDecl c)
    | DeclDataType (DataTypeDecl c)
    | DeclVal (ValDecl c)
    | DeclValBind (ValBind c)

deriving instance XEq c => Eq (Decl c)
deriving instance XShow c => Show (Decl c)


data TypeSigDecl c = TypeSigDecl
    {
        typeSigDeclCon :: Name,
        typeSigDeclType :: TypeExpr c
    }

deriving instance XEq c => Eq (TypeSigDecl c)
deriving instance XShow c => Show (TypeSigDecl c)


data ValSigDecl c = ValSigDecl
    {
        valSigDeclVar :: Name,
        valSigDeclType :: TypeExpr c
    }

deriving instance XEq c => Eq (ValSigDecl c)
deriving instance XShow c => Show (ValSigDecl c)


data ConSigDecl c = ConSigDecl
    {
        conSigDeclCon :: Name,
        conSigDeclType :: TypeExpr c
    }

deriving instance XEq c => Eq (ConSigDecl c)
deriving instance XShow c => Show (ConSigDecl c)


data TypeDecl c = TypeDecl
    {
        typeDeclVar :: DeclType c,
        typeDeclType :: TypeExpr c,
        typeDeclAssumptions :: [Decl c]
    }

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


data ValBind c = ValBind
    {
        valBindPat :: Pat c,
        valBindExpr :: Expr c,
        valBindAssumptions :: [Decl c]
    }

deriving instance XEq c => Eq (ValBind c)
deriving instance XShow c => Show (ValBind c)


data DeclType c
    = DeclAppType Name [BindVar c]
    | DeclInfixType (BindVar c) Name (BindVar c)

deriving instance XEq c => Eq (DeclType c)
deriving instance XShow c => Show (DeclType c)


data ImplType c
    = ImplAppType Name [TypeExpr c]
    | ImplInfixType (TypeExpr c) Name (TypeExpr c)

deriving instance XEq c => Eq (ImplType c)
deriving instance XShow c => Show (ImplType c)


data DeclExpr c
    = DeclAppExpr Name [BindVar c]
    | DeclInfixExpr (BindVar c) Name (BindVar c)

deriving instance XEq c => Eq (DeclExpr c)
deriving instance XShow c => Show (DeclExpr c)


data TypeExpr c
    = TypeForAll [BindVar c] (TypeExpr c)
    | TypeInfix (TypeExpr c) (TypeExpr c) (TypeExpr c)
    | TypeApp (TypeExpr c) [AppType c]
    | TypeSig (TypeExpr c) (TypeExpr c)
    | TypeLit (Lit c)
    | TypeTuple [TypeExpr c]
    | TypeArray [TypeExpr c]
    | TypeRecord [(Name, TypeExpr c)]

deriving instance XEq c => Eq (TypeExpr c)
deriving instance XShow c => Show (TypeExpr c)


data AppType c
    = AppType (TypeExpr c)
    | UnivAppType (TypeExpr c)

deriving instance XEq c => Eq (AppType c)
deriving instance XShow c => Show (AppType c)


data Expr c
    = ExprSig (Expr c) (TypeExpr c)
    | ExprInfix (Expr c) (Expr c) (Expr c)
    | ExprApp (Expr c) [AppExpr c]
    | ExprLambdaCase [CaseAlt c]
    | ExprLambda [Pat c] [GuardedAlt c]
    | ExprLetrec [Decl c] (Expr c)
    | ExprLet [Decl c] (Expr c)
    | ExprCase [Expr c] [CaseAlt c]
    | ExprDo [DoStmt c] (Expr c)
    | ExprCon Name
    | ExprVar Name
    | ExprLit (Lit c)
    | ExprTuple [Expr c]
    | ExprArray [Expr c]
    | ExprRecord [(Name, Expr c)]

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
    | LitInterpString [InterpStringPart c] (XLitInterpString c)

type family XLitRational c :: Type
type family XLitInteger c :: Type
type family XLitByteString c :: Type
type family XLitString c :: Type
type family XLitByteChar c :: Type
type family XLitChar c :: Type
type family XLitInterpString c :: Type

deriving instance XEq c => Eq (Lit c)
deriving instance XShow c => Show (Lit c)


data InterpStringPart c
    = InterpStringLit Text
    | InterpStringExpr (Expr c)

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
        f (XLitInterpString c)
    )

class XC Eq c => XEq c
class XC Show c => XShow c
