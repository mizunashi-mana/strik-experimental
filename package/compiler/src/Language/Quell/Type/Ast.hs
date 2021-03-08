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
  Name,
  mkName,
  BindVar (..),
  XBindVar,
  XUnivBindVar,
  XEq,
  XShow,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Data.TextId as TextId


data Program c = Program
    {
        decls      :: [Decl c]
    }
    deriving (Eq, Show)

data Decl c
    = DeclTypeSig (TypeSigDecl c)
    | DeclValSig (ValSigDecl c)
    | DeclConSig (ConSigDecl c)
    | DeclType (TypeDecl c)
    | DeclDataType (DataTypeDecl c)
    | DeclVal (ValDecl c)
    | DeclValBind (ValBind c)
    deriving (Eq, Show)

data TypeSigDecl c = TypeSigDecl
    {
        typeSigDeclCon :: Name,
        typeSigDeclType :: TypeExpr c
    }
    deriving (Eq, Show)

data ValSigDecl c = ValSigDecl
    {
        valSigDeclVar :: Name,
        valSigDeclType :: TypeExpr c
    }
    deriving (Eq, Show)

data ConSigDecl c = ConSigDecl
    {
        conSigDeclCon :: Name,
        conSigDeclType :: TypeExpr c
    }
    deriving (Eq, Show)

data TypeDecl c = TypeDecl
    {
        typeDeclVar :: DeclType c,
        typeDeclType :: TypeExpr c,
        typeDeclAssumptions :: [Decl c]
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data ValDecl c = ValDecl
    {
        valDeclVar :: DeclExpr c,
        valDeclExpr :: Expr c,
        valDeclAssumptions :: [Decl c]
    }
    deriving (Eq, Show)

data ValBind c = ValBind
    {
        valBindPat :: Pat c,
        valBindExpr :: Expr c,
        valBindAssumptions :: [Decl c]
    }
    deriving (Eq, Show)

data DeclType c
    = DeclAppType Name [BindVar c]
    | DeclInfixType (BindVar c) Name (BindVar c)
    deriving (Eq, Show)

data ImplType c
    = ImplAppType Name [TypeExpr c]
    | ImplInfixType (TypeExpr c) Name (TypeExpr c)
    deriving (Eq, Show)

data DeclExpr c
    = DeclAppExpr Name [BindVar c]
    | DeclInfixExpr (BindVar c) Name (BindVar c)
    deriving (Eq, Show)

data TypeExpr c
    = TypeForAll [BindVar c] (TypeExpr c)
    | TypeInfix (TypeExpr c) Name (TypeExpr c)
    | TypeApp (TypeExpr c) [AppType c]
    | TypeSig (TypeExpr c) (TypeExpr c)
    | TypeLit (Lit c)
    | TypeTuple [TypeExpr c]
    | TypeArray [TypeExpr c]
    | TypeRecord [(Name, TypeExpr c)]
    deriving (Eq, Show)

data AppType c
    = AppType (TypeExpr c)
    | UnivAppType (TypeExpr c)
    deriving (Eq, Show)

data Expr c
    = ExprSig (Expr c) (TypeExpr c)
    | ExprInfix (Expr c) Name (Expr c)
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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data GuardedAlt c = GuardedAlt
    {
        guardedAltGuard :: Maybe (Expr c),
        guardedAltExpr :: Expr c
    }
    deriving (Eq, Show)

data DoStmt c
    = DoStmtExpr (Expr c)
    | DoStmtBind (Pat c) (Expr c)
    | DoStmtLet [Decl c]
    | DoStmtLetrec [Decl c]
    deriving (Eq, Show)

data Pat c
    = PatSig (Pat c) (TypeExpr c)
    | PatOr [Pat c]
    | PatInfix (Pat c) Name (Pat c)
    | PatApp (Pat c) [AppPat c]
    | PatCon Name
    | PatVar Name
    | PatLit (Lit c)
    | PatTuple [Pat c]
    | PatArray [Pat c]
    | PatRecord [(Name, Pat c)]
    deriving (Eq, Show)

data AppPat c
    = AppPat (Pat c)
    | UnivAppPat (TypeExpr c)
    deriving (Eq, Show)

data Lit c
    = LitRational Rational
    | LitInteger Integer
    | LitByteString ByteString
    | LitString Text
    | LitByteChar Word8
    | LitChar Char
    | LitInterpString [InterpStringPart c]
    deriving (Eq, Show)

data InterpStringPart c
    = InterpStringLit Text
    | InterpStringExpr (Expr c)
    deriving (Eq, Show)

type Name = TextId.T

mkName :: Text -> Name
mkName n = TextId.textId n

data BindVar c
    = BindVar Name (Maybe (TypeExpr c)) (XBindVar c)
    | UnivBindVar Name (Maybe (TypeExpr c)) (XUnivBindVar c)

type family XBindVar c :: Type
type family XUnivBindVar c :: Type

deriving instance XEq c => Eq (BindVar c)
deriving instance XShow c => Show (BindVar c)


type XC :: (Type -> Constraint) -> Type -> Constraint
type XC f c =
    (
        f (XAppExpr c),
        f (XUnivAppExpr c),
        f (XBindVar c),
        f (XUnivBindVar c)
    )

class XC Eq c => XEq c
class XC Show c => XShow c
