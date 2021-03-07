module Language.Quell.Type.Ast (
  Program (..),
  Decl (..),
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Data.TextId as TextId


data Program c = Program
    {
        decls      :: [Decl c]
    }
    deriving (Eq, Show)

type Name = TextId.T

data Decl c
    = DeclTypeSig (TypeSigDecl c) (XTypeSigDecl c)
    | DeclValSig (ValSigDecl c) (XValSigDecl c)
    | DeclConSig (ConSigDecl c) (XConSigDecl c)
    | DeclType (TypeDecl c) (XTypeDecl c)
    | DeclDataType (DataTypeDecl c) (XDataTypeDecl c)
    | DeclVal (ValDecl c) (XValDecl c)
    | DeclValBind (ValBind c) (XValBind c)
    deriving (Eq, Show)

type family XTypeSigDecl c :: Type
type family XValSigDecl c :: Type
type family XConSigDecl c :: Type
type family XTypeDecl c :: Type
type family XDataTypeDecl c :: Type
type family XValDecl c :: Type
type family XValBind c :: Type

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
    = ImplAppType Name [Type c]
    | ImplInfixType (Type c) Name (Type c)
    deriving (Eq, Show)

data DeclExpr c
    = DeclAppExpr Name [BindVar c]
    | DeclInfixExpr (BindVar c) Name (BindVar c)
    deriving (Eq, Show)

data Type c
    = ForAllType [BindVar c] (Type c)
    | InfixType (Type c) Name (Type c)
    | AppType (Type c) [TypeApp c]
    | SigType (Type c) (Type c)
    | LitType (Literal c)
    | TupleType [Type c]
    | ArrayType [Type c]
    | RecordType [(Name, Type c)]
    deriving (Eq, Show)

data TypeApp c
    = TypeApp (Type c)
    | TypeUnivApp (Type c)
    deriving (Eq, Show)

data Expr c
    = SigExpr (Expr c) (Type c)
    | InfixExpr (Expr c) Name (Expr c)
    | AppExpr (Expr c) [ExprApp c]
    | LambdaCaseExpr [CaseAlt c]
    | LambdaExpr [Pat c] [GuardedAlt c]
    | LetrecExpr [Decl c] (Expr c)
    | LetExpr [Decl c] (Expr c)
    | CaseExpr [Expr c] [CaseAlt c]
    | DoExpr [DoStmt c] (Expr c)
    deriving (Eq, Show)

data ExprApp c
    = ExprApp (Expr c)
    | ExprUnivApp (Type c)
    deriving (Eq, Show)

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
    | DoStmtLet (Decl c)
    | DoStmtLetrec [Decl c]
    deriving (Eq, Show)
