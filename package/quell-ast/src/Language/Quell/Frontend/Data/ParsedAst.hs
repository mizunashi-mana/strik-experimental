{-# LANGUAGE UndecidableInstances #-}

module Language.Quell.Frontend.Data.ParsedAst where

import qualified Language.Quell.Data.TextId as TextId
import           Language.Quell.Prelude


data Program tag = Program {
    expr  :: Expr tag,
    extra :: XProgram tag
}

type family XProgram (tag :: a) :: Type

type MapXProgram :: (Type -> Constraint) -> a -> Constraint
type MapXProgram f tag =
    (
        f (XProgram tag)
    )
deriving instance EqXAll tag => Eq (Program tag)
deriving instance ShowXAll tag => Show (Program tag)


data Decl tag
    = DeclLocal [LocalDecl tag] (XDeclLocal tag)
    | DeclVar Name (Maybe (TypeExpr tag)) (Expr tag) (XDeclVar tag)
    | DeclTypeVar Name (Maybe (TypeExpr tag)) (TypeExpr tag) (XDeclTypeVar tag)

type family XDeclLocal (tag :: a) :: Type
type family XDeclVar (tag :: a) :: Type
type family XDeclTypeVar (tag :: a) :: Type

type MapXDecl :: (Type -> Constraint) -> a -> Constraint
type MapXDecl f tag =
    (
        f (XDeclLocal tag),
        f (XDeclVar tag),
        f (XDeclTypeVar tag)
    )
deriving instance EqXAll tag => Eq (Decl tag)
deriving instance ShowXAll tag => Show (Decl tag)


data LocalDecl tag
    = DeclLet [LetItem tag] (XDeclLet tag)
    | DeclRec [LetItem tag] (XDeclRec tag)

type family XDeclLet (tag :: a) :: Type
type family XDeclRec (tag :: a) :: Type

type MapXLocalDecl :: (Type -> Constraint) -> a -> Constraint
type MapXLocalDecl f tag =
    (
        f (XDeclLet tag),
        f (XDeclRec tag)
    )
deriving instance EqXAll tag => Eq (LocalDecl tag)
deriving instance ShowXAll tag => Show (LocalDecl tag)


data LetItem tag
    = LetVarExpr Name (Expr tag) (XLetVarExpr tag)

type family XLetVarExpr (tag :: a) :: Type

type MapXLetItem :: (Type -> Constraint) -> a -> Constraint
type MapXLetItem f tag =
    (
        f (XLetVarExpr tag)
    )
deriving instance EqXAll tag => Eq (LetItem tag)
deriving instance ShowXAll tag => Show (LetItem tag)


data Expr tag
    = ExprWithDecl (Expr tag) (Decl tag) (XExprWithDecl tag)
    | ExprWithAnn (Expr tag) (TypeExpr tag) (XExprWithAnn tag)
    | ExprInfix (Expr tag) [InfixAppExpr tag] (XExprInfix tag)
    | ExprApp (Expr tag) [AppExpr tag] (XExprApp tag)
    | ExprAbs (Expr tag) (XExprAbs tag)
    | ExprMatch [Expr tag] (Expr tag) (XExprMatch tag)
    | ExprCase [CaseItem tag] (XExprCase tag)
    | ExprIf [CaseItem tag] (XExprIf tag)
    | ExprBlock [BlockStmt tag] (XExprBlock tag)
    | ExprBlockBranch [BlockItem tag] (XExprBlockBranch tag)
    | ExprLiteral (Literal tag) (XExprLiteral tag)
    | ExprInterpString [InterpStringItem tag] (XExprInterpString tag)
    | ExprTuple [ExprTupleItem tag] (XExprTuple tag)
    | ExprVar Name (XExprVar tag)

type family XExprWithDecl (tag :: a) :: Type
type family XExprWithAnn (tag :: a) :: Type
type family XExprInfix (tag :: a) :: Type
type family XExprApp (tag :: a) :: Type
type family XExprAbs (tag :: a) :: Type
type family XExprMatch (tag :: a) :: Type
type family XExprCase (tag :: a) :: Type
type family XExprIf (tag :: a) :: Type
type family XExprBlock (tag :: a) :: Type
type family XExprBlockBranch (tag :: a) :: Type
type family XExprLiteral (tag :: a) :: Type
type family XExprInterpString (tag :: a) :: Type
type family XExprTuple (tag :: a) :: Type
type family XExprVar (tag :: a) :: Type

type MapXExpr :: (Type -> Constraint) -> a -> Constraint
type MapXExpr f tag =
    (
        f (XExprWithDecl tag),
        f (XExprWithAnn tag),
        f (XExprInfix tag),
        f (XExprApp tag),
        f (XExprAbs tag),
        f (XExprMatch tag),
        f (XExprCase tag),
        f (XExprIf tag),
        f (XExprBlock tag),
        f (XExprBlockBranch tag),
        f (XExprLiteral tag),
        f (XExprInterpString tag),
        f (XExprTuple tag),
        f (XExprVar tag)
    )
deriving instance EqXAll tag => Eq (Expr tag)
deriving instance ShowXAll tag => Show (Expr tag)


data InfixAppExpr tag = InfixAppExpr (Expr tag) (Expr tag) (XInfixAppExpr tag)

type family XInfixAppExpr (tag :: a) :: Type

type MapXInfixAppExpr :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppExpr f tag =
    (
        f (XInfixAppExpr tag)
    )
deriving instance EqXAll tag => Eq (InfixAppExpr tag)
deriving instance ShowXAll tag => Show (InfixAppExpr tag)


data AppExpr tag = AppExpr (Expr tag) (XAppExpr tag)

type family XAppExpr (tag :: a) :: Type

type MapXAppExpr :: (Type -> Constraint) -> a -> Constraint
type MapXAppExpr f tag =
    (
        f (XAppExpr tag)
    )
deriving instance EqXAll tag => Eq (AppExpr tag)
deriving instance ShowXAll tag => Show (AppExpr tag)


data CaseItem tag = CaseItem (View tag) (Expr tag) (XCaseItem tag)

type family XCaseItem (tag :: a) :: Type

type MapXCaseItem :: (Type -> Constraint) -> a -> Constraint
type MapXCaseItem f tag =
    (
        f (XCaseItem tag)
    )
deriving instance EqXAll tag => Eq (CaseItem tag)
deriving instance ShowXAll tag => Show (CaseItem tag)


data BlockStmt tag
    = BlockStmtExpr (Expr tag) (XBlockStmtExpr tag)
    | BlockStmtLocal (LocalDecl tag) (XBlockStmtLocal tag)

type family XBlockStmtExpr (tag :: a) :: Type
type family XBlockStmtLocal (tag :: a) :: Type

type MapXBlockStmt :: (Type -> Constraint) -> a -> Constraint
type MapXBlockStmt f tag =
    (
        f (XBlockStmtExpr tag),
        f (XBlockStmtLocal tag)
    )
deriving instance EqXAll tag => Eq (BlockStmt tag)
deriving instance ShowXAll tag => Show (BlockStmt tag)


data BlockItem tag = BlockItem [Pat tag] (Maybe (View tag)) (Expr tag) (XBlockItem tag)

type family XBlockItem (tag :: a) :: Type

type MapXBlockItem :: (Type -> Constraint) -> a -> Constraint
type MapXBlockItem f tag =
    (
        f (XBlockItem tag)
    )
deriving instance EqXAll tag => Eq (BlockItem tag)
deriving instance ShowXAll tag => Show (BlockItem tag)


data InterpStringItem tag
    = InterpStringItemPart Text (XInterpStringItemPart tag)
    | InterpStringItemExpr (Expr tag) (XInterpStringItemExpr tag)

type family XInterpStringItemPart (tag :: a) :: Type
type family XInterpStringItemExpr (tag :: a) :: Type

type MapXInterpStringItem :: (Type -> Constraint) -> a -> Constraint
type MapXInterpStringItem f tag =
    (
        f (XInterpStringItemPart tag),
        f (XInterpStringItemExpr tag)
    )
deriving instance EqXAll tag => Eq (InterpStringItem tag)
deriving instance ShowXAll tag => Show (InterpStringItem tag)


data ExprTupleItem tag
    = ExprTupleItemBindExpr Name (Expr tag) (XExprTupleItemBindExpr tag)
    | ExprTupleItemBindPromType Name (TypeExpr tag) (XExprTupleItemBindPromType tag)
    | ExprTupleItemExpr (Expr tag) (XExprTupleItemExpr tag)
    | ExprTupleItemPromType (TypeExpr tag) (XExprTupleItemPromType tag)
    | ExprTupleItemLocal (LocalDecl tag) (XExprTupleItemLocal tag)

type family XExprTupleItemBindExpr (tag :: a) :: Type
type family XExprTupleItemBindPromType (tag :: a) :: Type
type family XExprTupleItemExpr (tag :: a) :: Type
type family XExprTupleItemPromType (tag :: a) :: Type
type family XExprTupleItemLocal (tag :: a) :: Type

type MapXExprTupleItem :: (Type -> Constraint) -> a -> Constraint
type MapXExprTupleItem f tag =
    (
        f (XExprTupleItemBindExpr tag),
        f (XExprTupleItemBindPromType tag),
        f (XExprTupleItemExpr tag),
        f (XExprTupleItemPromType tag),
        f (XExprTupleItemLocal tag)
    )
deriving instance EqXAll tag => Eq (ExprTupleItem tag)
deriving instance ShowXAll tag => Show (ExprTupleItem tag)


data TypeExpr tag
    = TypeExprWithDecl (TypeExpr tag) [Decl tag] (XTypeExprWithDecl tag)
    | TypeExprWithAnn (TypeExpr tag) (TypeExpr tag) (XTypeExprWithAnn tag)
    | TypeExprInfix (TypeExpr tag) [InfixAppTypeExpr tag] (XTypeExprInfix tag)
    | TypeExprApp (TypeExpr tag) [AppTypeExpr tag] (XTypeExprApp tag)
    | TypeExprBlock [TypeBlockStmt tag] (XTypeExprBlock tag)
    | TypeExprLiteral (Literal tag) (XTypeExprLiteral tag)
    | TypeExprTuple [TypeTupleItem tag] (XTypeExprTuple tag)
    | TypeExprTupleSig [TypeTupleSigItem tag] (XTypeExprTupleSig tag)
    | TypeExprVar Name (XTypeExprVar tag)

type family XTypeExprWithDecl (tag :: a) :: Type
type family XTypeExprWithAnn (tag :: a) :: Type
type family XTypeExprInfix (tag :: a) :: Type
type family XTypeExprApp (tag :: a) :: Type
type family XTypeExprBlock (tag :: a) :: Type
type family XTypeExprLiteral (tag :: a) :: Type
type family XTypeExprTuple (tag :: a) :: Type
type family XTypeExprTupleSig (tag :: a) :: Type
type family XTypeExprVar (tag :: a) :: Type

type MapXTypeExpr :: (Type -> Constraint) -> a -> Constraint
type MapXTypeExpr f tag =
    (
        f (XTypeExprWithDecl tag),
        f (XTypeExprWithAnn tag),
        f (XTypeExprInfix tag),
        f (XTypeExprApp tag),
        f (XTypeExprBlock tag),
        f (XTypeExprLiteral tag),
        f (XTypeExprTuple tag),
        f (XTypeExprTupleSig tag),
        f (XTypeExprVar tag)
    )
deriving instance EqXAll tag => Eq (TypeExpr tag)
deriving instance ShowXAll tag => Show (TypeExpr tag)


data InfixAppTypeExpr tag = InfixAppTypeExpr (TypeExpr tag) (TypeExpr tag) (XInfixAppTypeExpr tag)

type family XInfixAppTypeExpr (tag :: a) :: Type

type MapXInfixAppTypeExpr :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppTypeExpr f tag =
    (
        f (XInfixAppTypeExpr tag)
    )
deriving instance EqXAll tag => Eq (InfixAppTypeExpr tag)
deriving instance ShowXAll tag => Show (InfixAppTypeExpr tag)


data AppTypeExpr tag = AppTypeExpr (TypeExpr tag) (XAppTypeExpr tag)

type family XAppTypeExpr (tag :: a) :: Type

type MapXAppTypeExpr :: (Type -> Constraint) -> a -> Constraint
type MapXAppTypeExpr f tag =
    (
        f (XAppTypeExpr tag)
    )
deriving instance EqXAll tag => Eq (AppTypeExpr tag)
deriving instance ShowXAll tag => Show (AppTypeExpr tag)


data TypeBlockStmt tag
    = TypeBlockStmtType (TypeExpr tag) (XTypeBlockStmtType tag)
    | TypeBlockStmtLocal (LocalDecl tag) (XTypeBlockStmtLocal tag)

type family XTypeBlockStmtType (tag :: a) :: Type
type family XTypeBlockStmtLocal (tag :: a) :: Type

type MapXTypeBlockStmt :: (Type -> Constraint) -> a -> Constraint
type MapXTypeBlockStmt f tag =
    (
        f (XTypeBlockStmtType tag),
        f (XTypeBlockStmtLocal tag)
    )
deriving instance EqXAll tag => Eq (TypeBlockStmt tag)
deriving instance ShowXAll tag => Show (TypeBlockStmt tag)


data TypeTupleItem tag
    = TypeTupleItemBindPromType Name (Maybe (TypeExpr tag)) (TypeExpr tag) (XTypeTupleItemBindPromType tag)
    | TypeTupleItemBindType Name (Maybe (TypeExpr tag)) (TypeExpr tag) (XTypeTupleItemBindType tag)
    | TypeTupleItemPromType (TypeExpr tag) (XTypeTupleItemPromType tag)
    | TypeTupleItemType (TypeExpr tag) (XTypeTupleItemType tag)
    | TypeTupleItemLocal (LocalDecl tag) (XTypeTupleItemLocal tag)

type family XTypeTupleItemBindPromType (tag :: a) :: Type
type family XTypeTupleItemBindType (tag :: a) :: Type
type family XTypeTupleItemPromType (tag :: a) :: Type
type family XTypeTupleItemType (tag :: a) :: Type
type family XTypeTupleItemLocal (tag :: a) :: Type

type MapXTypeTupleItem :: (Type -> Constraint) -> a -> Constraint
type MapXTypeTupleItem f tag =
    (
        f (XTypeTupleItemBindPromType tag),
        f (XTypeTupleItemBindType tag),
        f (XTypeTupleItemPromType tag),
        f (XTypeTupleItemType tag),
        f (XTypeTupleItemLocal tag)
    )
deriving instance EqXAll tag => Eq (TypeTupleItem tag)
deriving instance ShowXAll tag => Show (TypeTupleItem tag)


data TypeTupleSigItem tag
    = TypeTupleSigItemNamedPromType Name (TypeExpr tag) (XTypeTupleSigItemNamedPromType tag)
    | TypeTupleSigItemNamedType Name (TypeExpr tag) (XTypeTupleSigItemNamedType tag)
    | TypeTupleSigItemPromType (TypeExpr tag) (XTypeTupleSigItemPromType tag)
    | TypeTupleSigItemType (TypeExpr tag) (XTypeTupleSigItemType tag)
    | TypeTupleSigItemLocal (LocalDecl tag) (XTypeTupleSigItemLocal tag)

type family XTypeTupleSigItemNamedPromType (tag :: a) :: Type
type family XTypeTupleSigItemNamedType (tag :: a) :: Type
type family XTypeTupleSigItemPromType (tag :: a) :: Type
type family XTypeTupleSigItemType (tag :: a) :: Type
type family XTypeTupleSigItemLocal (tag :: a) :: Type

type MapXTypeTupleSigItem :: (Type -> Constraint) -> a -> Constraint
type MapXTypeTupleSigItem f tag =
    (
        f (XTypeTupleSigItemNamedPromType tag),
        f (XTypeTupleSigItemNamedType tag),
        f (XTypeTupleSigItemPromType tag),
        f (XTypeTupleSigItemType tag),
        f (XTypeTupleSigItemLocal tag)
    )
deriving instance EqXAll tag => Eq (TypeTupleSigItem tag)
deriving instance ShowXAll tag => Show (TypeTupleSigItem tag)


data Pat tag
    = PatWithAnn (Pat tag) (TypeExpr tag) (XPatWithAnn tag)
    | PatInfix (Pat tag) [InfixAppPat tag] (XPatInfix tag)
    | PatApp Name [AppPat tag] (XPatApp tag)
    | PatLiteral (Literal tag) (XPatLiteral tag)
    | PatTuple [PatTupleItem tag] (XPatTuple tag)
    | PatVar Name (XPatVar tag)

type family XPatWithAnn (tag :: a) :: Type
type family XPatInfix (tag :: a) :: Type
type family XPatApp (tag :: a) :: Type
type family XPatLiteral (tag :: a) :: Type
type family XPatTuple (tag :: a) :: Type
type family XPatVar (tag :: a) :: Type

type MapXPat :: (Type -> Constraint) -> a -> Constraint
type MapXPat f tag =
    (
        f (XPatWithAnn tag),
        f (XPatInfix tag),
        f (XPatApp tag),
        f (XPatLiteral tag),
        f (XPatTuple tag),
        f (XPatVar tag)
    )
deriving instance EqXAll tag => Eq (Pat tag)
deriving instance ShowXAll tag => Show (Pat tag)


data InfixAppPat tag = InfixAppPat Name (Pat tag) (XInfixAppPat tag)

type family XInfixAppPat (tag :: a) :: Type

type MapXInfixAppPat :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppPat f tag =
    (
        f (XInfixAppPat tag)
    )
deriving instance EqXAll tag => Eq (InfixAppPat tag)
deriving instance ShowXAll tag => Show (InfixAppPat tag)


data AppPat tag = AppPat (Pat tag) (XAppPat tag)

type family XAppPat (tag :: a) :: Type

type MapXAppPat :: (Type -> Constraint) -> a -> Constraint
type MapXAppPat f tag =
    (
        f (XAppPat tag)
    )
deriving instance EqXAll tag => Eq (AppPat tag)
deriving instance ShowXAll tag => Show (AppPat tag)


data PatTupleItem tag
    = PatTupleItemNamedPat Name (Pat tag) (XPatTupleItemNamedPat tag)
    | PatTupleItemPat (Pat tag) (XPatTupleItemPat tag)

type family XPatTupleItemNamedPat (tag :: a) :: Type
type family XPatTupleItemPat (tag :: a) :: Type

type MapXPatTupleItem :: (Type -> Constraint) -> a -> Constraint
type MapXPatTupleItem f tag =
    (
        f (XPatTupleItemNamedPat tag),
        f (XPatTupleItemPat tag)
    )
deriving instance EqXAll tag => Eq (PatTupleItem tag)
deriving instance ShowXAll tag => Show (PatTupleItem tag)


data View tag
    = ViewBlock [View tag] (XViewBlock tag)
    | ViewLetDecl (Pat tag) (Expr tag) (XViewLetDecl tag)
    | ViewExpr (Expr tag) (XViewExpr tag)

type family XViewBlock (tag :: a) :: Type
type family XViewLetDecl (tag :: a) :: Type
type family XViewExpr (tag :: a) :: Type

type MapXView :: (Type -> Constraint) -> a -> Constraint
type MapXView f tag =
    (
        f (XViewBlock tag),
        f (XViewLetDecl tag),
        f (XViewExpr tag)
    )
deriving instance EqXAll tag => Eq (View tag)
deriving instance ShowXAll tag => Show (View tag)


data Literal tag
    = LitString Text (XLitString tag)
    | LitRational Rational (XLitRational tag)
    | LitInteger Integer (XLitInteger tag)

type family XLitString (tag :: a) :: Type
type family XLitRational (tag :: a) :: Type
type family XLitInteger (tag :: a) :: Type

type MapXLiteral :: (Type -> Constraint) -> a -> Constraint
type MapXLiteral f tag =
    (
        f (XLitString tag),
        f (XLitRational tag),
        f (XLitInteger tag)
    )
deriving instance EqXAll tag => Eq (Literal tag)
deriving instance ShowXAll tag => Show (Literal tag)


type Name = TextId.T


type MapXAll :: (Type -> Constraint) -> a -> Constraint
type MapXAll f tag =
    (
        MapXProgram f tag,
        MapXDecl f tag,
        MapXLocalDecl f tag,
        MapXLetItem f tag,
        MapXExpr f tag,
        MapXInfixAppExpr f tag,
        MapXAppExpr f tag,
        MapXCaseItem f tag,
        MapXBlockStmt f tag,
        MapXBlockItem f tag,
        MapXInterpStringItem f tag,
        MapXExprTupleItem f tag,
        MapXTypeExpr f tag,
        MapXInfixAppTypeExpr f tag,
        MapXAppTypeExpr f tag,
        MapXTypeBlockStmt f tag,
        MapXTypeTupleItem f tag,
        MapXTypeTupleSigItem f tag,
        MapXPat f tag,
        MapXInfixAppPat f tag,
        MapXAppPat f tag,
        MapXPatTupleItem f tag,
        MapXView f tag,
        MapXLiteral f tag
    )

class MapXAll Eq tag => EqXAll tag
class MapXAll Show tag => ShowXAll tag


data Bundle a

type instance XProgram (Bundle a) = a
type instance XDeclLocal (Bundle a) = a
type instance XDeclLet (Bundle a) = a
type instance XDeclRec (Bundle a) = a
type instance XDeclTypeVar (Bundle a) = a
type instance XDeclVar (Bundle a) = a
type instance XLetVarExpr (Bundle a) = a
type instance XExprWithDecl (Bundle a) = a
type instance XExprWithAnn (Bundle a) = a
type instance XExprInfix (Bundle a) = a
type instance XExprApp (Bundle a) = a
type instance XExprAbs (Bundle a) = a
type instance XExprMatch (Bundle a) = a
type instance XExprCase (Bundle a) = a
type instance XExprIf (Bundle a) = a
type instance XExprBlock (Bundle a) = a
type instance XExprBlockBranch (Bundle a) = a
type instance XExprLiteral (Bundle a) = a
type instance XExprInterpString (Bundle a) = a
type instance XExprTuple (Bundle a) = a
type instance XExprVar (Bundle a) = a
type instance XInfixAppExpr (Bundle a) = a
type instance XAppExpr (Bundle a) = a
type instance XCaseItem (Bundle a) = a
type instance XBlockStmtExpr (Bundle a) = a
type instance XBlockStmtLocal (Bundle a) = a
type instance XBlockItem (Bundle a) = a
type instance XInterpStringItemPart (Bundle a) = a
type instance XInterpStringItemExpr (Bundle a) = a
type instance XExprTupleItemBindPromType (Bundle a) = a
type instance XExprTupleItemBindExpr (Bundle a) = a
type instance XExprTupleItemPromType (Bundle a) = a
type instance XExprTupleItemExpr (Bundle a) = a
type instance XExprTupleItemLocal (Bundle a) = a
type instance XTypeExprWithDecl (Bundle a) = a
type instance XTypeExprWithAnn (Bundle a) = a
type instance XTypeExprInfix (Bundle a) = a
type instance XTypeExprApp (Bundle a) = a
type instance XTypeExprBlock (Bundle a) = a
type instance XTypeExprLiteral (Bundle a) = a
type instance XTypeExprTuple (Bundle a) = a
type instance XTypeExprTupleSig (Bundle a) = a
type instance XTypeExprVar (Bundle a) = a
type instance XInfixAppTypeExpr (Bundle a) = a
type instance XAppTypeExpr (Bundle a) = a
type instance XTypeBlockStmtType (Bundle a) = a
type instance XTypeBlockStmtLocal (Bundle a) = a
type instance XTypeTupleItemBindPromType (Bundle a) = a
type instance XTypeTupleItemBindType (Bundle a) = a
type instance XTypeTupleItemPromType (Bundle a) = a
type instance XTypeTupleItemType (Bundle a) = a
type instance XTypeTupleItemLocal (Bundle a) = a
type instance XTypeTupleSigItemNamedPromType (Bundle a) = a
type instance XTypeTupleSigItemNamedType (Bundle a) = a
type instance XTypeTupleSigItemPromType (Bundle a) = a
type instance XTypeTupleSigItemType (Bundle a) = a
type instance XTypeTupleSigItemLocal (Bundle a) = a
type instance XPatWithAnn (Bundle a) = a
type instance XPatInfix (Bundle a) = a
type instance XPatApp (Bundle a) = a
type instance XPatLiteral (Bundle a) = a
type instance XPatTuple (Bundle a) = a
type instance XPatVar (Bundle a) = a
type instance XInfixAppPat (Bundle a) = a
type instance XAppPat (Bundle a) = a
type instance XPatTupleItemNamedPat (Bundle a) = a
type instance XPatTupleItemPat (Bundle a) = a
type instance XViewBlock (Bundle a) = a
type instance XViewLetDecl (Bundle a) = a
type instance XViewExpr (Bundle a) = a
type instance XLitString (Bundle a) = a
type instance XLitRational (Bundle a) = a
type instance XLitInteger (Bundle a) = a

instance Eq a => EqXAll (Bundle a)
instance Show a => ShowXAll (Bundle a)
