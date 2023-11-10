{-# LANGUAGE UndecidableInstances #-}

module Language.Strik.Frontend.Data.ParsedAst where

import           Language.Strik.Prelude

import qualified Language.Strik.Data.TextId as TextId


data Program tag = Program
    {
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

instance NormalizeXAll tag a => ExtractX tag a Program where
    extractX x = normalizeX @tag Proxy do extra x


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

instance NormalizeXAll tag a => ExtractX tag a Decl where
    extractX = \case
        DeclLocal _ x ->
            normalizeX @tag Proxy x
        DeclVar _ _ _ x ->
            normalizeX @tag Proxy x
        DeclTypeVar _ _ _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a LocalDecl where
    extractX = \case
        DeclLet _ x ->
            normalizeX @tag Proxy x
        DeclRec _ x ->
            normalizeX @tag Proxy x


data LetItem tag
    = LetVarExpr Name (Expr tag) (XLetVarExpr tag)
    | LetVarType Name (TypeExpr tag) (XLetVarType tag)

type family XLetVarExpr (tag :: a) :: Type
type family XLetVarType (tag :: a) :: Type

type MapXLetItem :: (Type -> Constraint) -> a -> Constraint
type MapXLetItem f tag =
    (
        f (XLetVarExpr tag),
        f (XLetVarType tag)
    )
deriving instance EqXAll tag => Eq (LetItem tag)
deriving instance ShowXAll tag => Show (LetItem tag)

instance NormalizeXAll tag a => ExtractX tag a LetItem where
    extractX = \case
        LetVarExpr _ _ x ->
            normalizeX @tag Proxy x
        LetVarType _ _ x ->
            normalizeX @tag Proxy x


data Expr tag
    = ExprWithDecl (Expr tag) (Decl tag) (XExprWithDecl tag)
    | ExprWithAnn (Expr tag) (TypeExpr tag) (XExprWithAnn tag)
    | ExprInfix (Expr tag) [InfixAppExpr tag] (XExprInfix tag)
    | ExprApp (Expr tag) [AppExpr tag] (XExprApp tag)
    | ExprAbs (Expr tag) (XExprAbs tag)
    | ExprMatch [Expr tag] (Expr tag) (XExprMatch tag)
    | ExprCase [CaseItem tag] (XExprCase tag)
    | ExprIf [CaseItem tag] (XExprIf tag)
    | ExprBlock [Block tag] (XExprBlock tag)
    | ExprLiteral (Literal tag) (XExprLiteral tag)
    | ExprInterpString [InterpStringItem tag] (XExprInterpString tag)
    | ExprTuple (Tuple tag) (XExprTuple tag)
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
        f (XExprLiteral tag),
        f (XExprInterpString tag),
        f (XExprTuple tag),
        f (XExprVar tag)
    )
deriving instance EqXAll tag => Eq (Expr tag)
deriving instance ShowXAll tag => Show (Expr tag)

instance NormalizeXAll tag a => ExtractX tag a Expr where
    extractX = \case
        ExprWithDecl _ _ x ->
            normalizeX @tag Proxy x
        ExprWithAnn _ _ x ->
            normalizeX @tag Proxy x
        ExprInfix _ _ x ->
            normalizeX @tag Proxy x
        ExprApp _ _ x ->
            normalizeX @tag Proxy x
        ExprAbs _ x ->
            normalizeX @tag Proxy x
        ExprMatch _ _ x ->
            normalizeX @tag Proxy x
        ExprCase _ x ->
            normalizeX @tag Proxy x
        ExprIf _ x ->
            normalizeX @tag Proxy x
        ExprBlock _ x ->
            normalizeX @tag Proxy x
        ExprLiteral _ x ->
            normalizeX @tag Proxy x
        ExprInterpString _ x ->
            normalizeX @tag Proxy x
        ExprTuple _ x ->
            normalizeX @tag Proxy x
        ExprVar _ x ->
            normalizeX @tag Proxy x


data InfixAppExpr tag = InfixAppExpr (Expr tag) (Expr tag) (XInfixAppExpr tag)

type family XInfixAppExpr (tag :: a) :: Type

type MapXInfixAppExpr :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppExpr f tag =
    (
        f (XInfixAppExpr tag)
    )
deriving instance EqXAll tag => Eq (InfixAppExpr tag)
deriving instance ShowXAll tag => Show (InfixAppExpr tag)

instance NormalizeXAll tag a => ExtractX tag a InfixAppExpr where
    extractX = \case
        InfixAppExpr _ _ x ->
            normalizeX @tag Proxy x


data AppExpr tag = AppExpr (Expr tag) (XAppExpr tag)

type family XAppExpr (tag :: a) :: Type

type MapXAppExpr :: (Type -> Constraint) -> a -> Constraint
type MapXAppExpr f tag =
    (
        f (XAppExpr tag)
    )
deriving instance EqXAll tag => Eq (AppExpr tag)
deriving instance ShowXAll tag => Show (AppExpr tag)

instance NormalizeXAll tag a => ExtractX tag a AppExpr where
    extractX = \case
        AppExpr _ x ->
            normalizeX @tag Proxy x


data CaseItem tag = CaseItem (View tag) (Expr tag) (XCaseItem tag)

type family XCaseItem (tag :: a) :: Type

type MapXCaseItem :: (Type -> Constraint) -> a -> Constraint
type MapXCaseItem f tag =
    (
        f (XCaseItem tag)
    )
deriving instance EqXAll tag => Eq (CaseItem tag)
deriving instance ShowXAll tag => Show (CaseItem tag)

instance NormalizeXAll tag a => ExtractX tag a CaseItem where
    extractX = \case
        CaseItem _ _ x ->
            normalizeX @tag Proxy x


data Block tag
    = Block [BlockStmt tag] (XBlock tag)
    | BlockBranch [BlockItem tag] (XBlockBranch tag)

type family XBlock (tag :: a) :: Type
type family XBlockBranch (tag :: a) :: Type

type MapXBlock :: (Type -> Constraint) -> a -> Constraint
type MapXBlock f tag =
    (
        f (XBlock tag),
        f (XBlockBranch tag)
    )
deriving instance EqXAll tag => Eq (Block tag)
deriving instance ShowXAll tag => Show (Block tag)

instance NormalizeXAll tag a => ExtractX tag a Block where
    extractX = \case
        Block _ x ->
            normalizeX @tag Proxy x
        BlockBranch _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a BlockStmt where
    extractX = \case
        BlockStmtExpr _ x ->
            normalizeX @tag Proxy x
        BlockStmtLocal _ x ->
            normalizeX @tag Proxy x


data BlockItem tag = BlockItem [Pat tag] (Maybe (View tag)) (Expr tag) (XBlockItem tag)

type family XBlockItem (tag :: a) :: Type

type MapXBlockItem :: (Type -> Constraint) -> a -> Constraint
type MapXBlockItem f tag =
    (
        f (XBlockItem tag)
    )
deriving instance EqXAll tag => Eq (BlockItem tag)
deriving instance ShowXAll tag => Show (BlockItem tag)

instance NormalizeXAll tag a => ExtractX tag a BlockItem where
    extractX = \case
        BlockItem _ _ _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a InterpStringItem where
    extractX = \case
        InterpStringItemPart _ x ->
            normalizeX @tag Proxy x
        InterpStringItemExpr _ x ->
            normalizeX @tag Proxy x


data Tuple tag = Tuple [TupleItem tag] (XTuple tag)

type family XTuple (tag :: a) :: Type

type MapXTuple :: (Type -> Constraint) -> a -> Constraint
type MapXTuple f tag =
    (
        f (XTuple tag)
    )
deriving instance EqXAll tag => Eq (Tuple tag)
deriving instance ShowXAll tag => Show (Tuple tag)

instance NormalizeXAll tag a => ExtractX tag a Tuple where
    extractX = \case
        Tuple _ x ->
            normalizeX @tag Proxy x


data TupleItem tag
    = TupleItemBindExpr Name (Expr tag) (XTupleItemBindExpr tag)
    | TupleItemBindPromType Name (TypeExpr tag) (XTupleItemBindPromType tag)
    | TupleItemExpr (Expr tag) (XTupleItemExpr tag)
    | TupleItemPromType (TypeExpr tag) (XTupleItemPromType tag)
    | TupleItemLocal (LocalDecl tag) (XTupleItemLocal tag)

type family XTupleItemBindExpr (tag :: a) :: Type
type family XTupleItemBindPromType (tag :: a) :: Type
type family XTupleItemExpr (tag :: a) :: Type
type family XTupleItemPromType (tag :: a) :: Type
type family XTupleItemLocal (tag :: a) :: Type

type MapXTupleItem :: (Type -> Constraint) -> a -> Constraint
type MapXTupleItem f tag =
    (
        f (XTupleItemBindExpr tag),
        f (XTupleItemBindPromType tag),
        f (XTupleItemExpr tag),
        f (XTupleItemPromType tag),
        f (XTupleItemLocal tag)
    )
deriving instance EqXAll tag => Eq (TupleItem tag)
deriving instance ShowXAll tag => Show (TupleItem tag)

instance NormalizeXAll tag a => ExtractX tag a TupleItem where
    extractX = \case
        TupleItemBindExpr _ _ x ->
            normalizeX @tag Proxy x
        TupleItemBindPromType _ _ x ->
            normalizeX @tag Proxy x
        TupleItemExpr _ x ->
            normalizeX @tag Proxy x
        TupleItemPromType _ x ->
            normalizeX @tag Proxy x
        TupleItemLocal _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a TypeExpr where
    extractX = \case
        TypeExprWithDecl _ _ x ->
            normalizeX @tag Proxy x
        TypeExprWithAnn _ _ x ->
            normalizeX @tag Proxy x
        TypeExprInfix _ _ x ->
            normalizeX @tag Proxy x
        TypeExprApp _ _ x ->
            normalizeX @tag Proxy x
        TypeExprBlock _ x ->
            normalizeX @tag Proxy x
        TypeExprLiteral _ x ->
            normalizeX @tag Proxy x
        TypeExprTuple _ x ->
            normalizeX @tag Proxy x
        TypeExprTupleSig _ x ->
            normalizeX @tag Proxy x
        TypeExprVar _ x ->
            normalizeX @tag Proxy x


data InfixAppTypeExpr tag = InfixAppTypeExpr (TypeExpr tag) (TypeExpr tag) (XInfixAppTypeExpr tag)

type family XInfixAppTypeExpr (tag :: a) :: Type

type MapXInfixAppTypeExpr :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppTypeExpr f tag =
    (
        f (XInfixAppTypeExpr tag)
    )
deriving instance EqXAll tag => Eq (InfixAppTypeExpr tag)
deriving instance ShowXAll tag => Show (InfixAppTypeExpr tag)

instance NormalizeXAll tag a => ExtractX tag a InfixAppTypeExpr where
    extractX = \case
        InfixAppTypeExpr _ _ x ->
            normalizeX @tag Proxy x


data AppTypeExpr tag = AppTypeExpr (TypeExpr tag) (XAppTypeExpr tag)

type family XAppTypeExpr (tag :: a) :: Type

type MapXAppTypeExpr :: (Type -> Constraint) -> a -> Constraint
type MapXAppTypeExpr f tag =
    (
        f (XAppTypeExpr tag)
    )
deriving instance EqXAll tag => Eq (AppTypeExpr tag)
deriving instance ShowXAll tag => Show (AppTypeExpr tag)

instance NormalizeXAll tag a => ExtractX tag a AppTypeExpr where
    extractX = \case
        AppTypeExpr _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a TypeBlockStmt where
    extractX = \case
        TypeBlockStmtType _ x ->
            normalizeX @tag Proxy x
        TypeBlockStmtLocal _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a TypeTupleItem where
    extractX = \case
        TypeTupleItemBindPromType _ _ _ x ->
            normalizeX @tag Proxy x
        TypeTupleItemBindType _ _ _ x ->
            normalizeX @tag Proxy x
        TypeTupleItemPromType _ x ->
            normalizeX @tag Proxy x
        TypeTupleItemType _ x ->
            normalizeX @tag Proxy x
        TypeTupleItemLocal _ x ->
            normalizeX @tag Proxy x


data TypeTupleSigItem tag
    = TypeTupleSigItemNamedPromType Name (TypeExpr tag) (XTypeTupleSigItemNamedPromType tag)
    | TypeTupleSigItemNamedType Name (TypeExpr tag) (XTypeTupleSigItemNamedType tag)
    | TypeTupleSigItemPromType (TypeExpr tag) (XTypeTupleSigItemPromType tag)
    | TypeTupleSigItemType (TypeExpr tag) (XTypeTupleSigItemType tag)

type family XTypeTupleSigItemNamedPromType (tag :: a) :: Type
type family XTypeTupleSigItemNamedType (tag :: a) :: Type
type family XTypeTupleSigItemPromType (tag :: a) :: Type
type family XTypeTupleSigItemType (tag :: a) :: Type

type MapXTypeTupleSigItem :: (Type -> Constraint) -> a -> Constraint
type MapXTypeTupleSigItem f tag =
    (
        f (XTypeTupleSigItemNamedPromType tag),
        f (XTypeTupleSigItemNamedType tag),
        f (XTypeTupleSigItemPromType tag),
        f (XTypeTupleSigItemType tag)
    )
deriving instance EqXAll tag => Eq (TypeTupleSigItem tag)
deriving instance ShowXAll tag => Show (TypeTupleSigItem tag)

instance NormalizeXAll tag a => ExtractX tag a TypeTupleSigItem where
    extractX = \case
        TypeTupleSigItemNamedPromType _ _ x ->
            normalizeX @tag Proxy x
        TypeTupleSigItemNamedType _ _ x ->
            normalizeX @tag Proxy x
        TypeTupleSigItemPromType _ x ->
            normalizeX @tag Proxy x
        TypeTupleSigItemType _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a Pat where
    extractX = \case
        PatWithAnn _ _ x ->
            normalizeX @tag Proxy x
        PatInfix _ _ x ->
            normalizeX @tag Proxy x
        PatApp _ _ x ->
            normalizeX @tag Proxy x
        PatLiteral _ x ->
            normalizeX @tag Proxy x
        PatTuple _ x ->
            normalizeX @tag Proxy x
        PatVar _ x ->
            normalizeX @tag Proxy x


data InfixAppPat tag = InfixAppPat Name (Pat tag) (XInfixAppPat tag)

type family XInfixAppPat (tag :: a) :: Type

type MapXInfixAppPat :: (Type -> Constraint) -> a -> Constraint
type MapXInfixAppPat f tag =
    (
        f (XInfixAppPat tag)
    )
deriving instance EqXAll tag => Eq (InfixAppPat tag)
deriving instance ShowXAll tag => Show (InfixAppPat tag)

instance NormalizeXAll tag a => ExtractX tag a InfixAppPat where
    extractX = \case
        InfixAppPat _ _ x ->
            normalizeX @tag Proxy x


data AppPat tag = AppPat (Pat tag) (XAppPat tag)

type family XAppPat (tag :: a) :: Type

type MapXAppPat :: (Type -> Constraint) -> a -> Constraint
type MapXAppPat f tag =
    (
        f (XAppPat tag)
    )
deriving instance EqXAll tag => Eq (AppPat tag)
deriving instance ShowXAll tag => Show (AppPat tag)

instance NormalizeXAll tag a => ExtractX tag a AppPat where
    extractX = \case
        AppPat _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a PatTupleItem where
    extractX = \case
        PatTupleItemNamedPat _ _ x ->
            normalizeX @tag Proxy x
        PatTupleItemPat _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a View where
    extractX = \case
        ViewBlock _ x ->
            normalizeX @tag Proxy x
        ViewLetDecl _ _ x ->
            normalizeX @tag Proxy x
        ViewExpr _ x ->
            normalizeX @tag Proxy x


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

instance NormalizeXAll tag a => ExtractX tag a Literal where
    extractX = \case
        LitString _ x ->
            normalizeX @tag Proxy x
        LitRational _ x ->
            normalizeX @tag Proxy x
        LitInteger _ x ->
            normalizeX @tag Proxy x


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
        MapXBlock f tag,
        MapXBlockStmt f tag,
        MapXBlockItem f tag,
        MapXInterpStringItem f tag,
        MapXTuple f tag,
        MapXTupleItem f tag,
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

class NormalizeX tag a x | tag -> a where
    normalizeX :: Proxy tag -> x -> a

class MapXAll (NormalizeX tag a) tag => NormalizeXAll tag a

class NormalizeXAll tag a => ExtractX tag a f where
    extractX :: f tag -> a


data Bundle a

type instance XProgram (Bundle a) = a
type instance XDeclLocal (Bundle a) = a
type instance XDeclLet (Bundle a) = a
type instance XDeclRec (Bundle a) = a
type instance XDeclTypeVar (Bundle a) = a
type instance XDeclVar (Bundle a) = a
type instance XLetVarExpr (Bundle a) = a
type instance XLetVarType (Bundle a) = a
type instance XExprWithDecl (Bundle a) = a
type instance XExprWithAnn (Bundle a) = a
type instance XExprInfix (Bundle a) = a
type instance XExprApp (Bundle a) = a
type instance XExprAbs (Bundle a) = a
type instance XExprMatch (Bundle a) = a
type instance XExprCase (Bundle a) = a
type instance XExprIf (Bundle a) = a
type instance XExprBlock (Bundle a) = a
type instance XExprLiteral (Bundle a) = a
type instance XExprInterpString (Bundle a) = a
type instance XExprTuple (Bundle a) = a
type instance XExprVar (Bundle a) = a
type instance XInfixAppExpr (Bundle a) = a
type instance XAppExpr (Bundle a) = a
type instance XCaseItem (Bundle a) = a
type instance XBlock (Bundle a) = a
type instance XBlockBranch (Bundle a) = a
type instance XBlockStmtExpr (Bundle a) = a
type instance XBlockStmtLocal (Bundle a) = a
type instance XBlockItem (Bundle a) = a
type instance XInterpStringItemPart (Bundle a) = a
type instance XInterpStringItemExpr (Bundle a) = a
type instance XTuple (Bundle a) = a
type instance XTupleItemBindPromType (Bundle a) = a
type instance XTupleItemBindExpr (Bundle a) = a
type instance XTupleItemPromType (Bundle a) = a
type instance XTupleItemExpr (Bundle a) = a
type instance XTupleItemLocal (Bundle a) = a
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

instance NormalizeX (Bundle a) a a where
    normalizeX _ x = x

instance NormalizeXAll (Bundle a) a
