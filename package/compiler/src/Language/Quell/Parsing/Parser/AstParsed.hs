{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Parser.AstParsed (
    T,
    AstParsed,

    SpanBuilder (..),
    MaySpanBuilder (..),
    spn,
    MaySideSpan (..),
) where

import           Language.Quell.Prelude

import qualified Language.Parser.Ptera.TH.Class.LiftType as LiftType
import qualified Language.Quell.Parsing.Spanned          as Spanned
import qualified Language.Quell.Type.Ast                 as Ast


class SpanBuilder s where
    sp :: s -> Spanned.Span

class MaySpanBuilder s where
    maySp :: s -> Maybe Spanned.Span

spn :: SpanBuilder s => s -> a -> Spanned.Spanned a
spn s x = Spanned.Spanned
    {
        getSpan = sp s,
        unSpanned = x
    }

instance SpanBuilder Spanned.Span where
    sp s = s

instance SpanBuilder (Spanned.Spanned a) where
    sp sx = Spanned.getSpan sx

instance SpanBuilder s => SpanBuilder (NonEmpty s) where
    sp l = sconcat do l <&> \s -> sp s

instance (SpanBuilder s1, SpanBuilder s2) => SpanBuilder (s1, s2) where
    sp (s1, s2) = sp s1 <> sp s2

instance (SpanBuilder s1, SpanBuilder s2, SpanBuilder s3)
        => SpanBuilder (s1, s2, s3) where
    sp (s1, s2, s3) = sp s1 <> sp s2 <> sp s3

instance (SpanBuilder s1, SpanBuilder s2, SpanBuilder s3, SpanBuilder s4)
        => SpanBuilder (s1, s2, s3, s4) where
    sp (s1, s2, s3, s4) = sp s1 <> sp s2 <> sp s3 <> sp s4

instance (
            SpanBuilder s1,
            SpanBuilder s2,
            SpanBuilder s3,
            SpanBuilder s4,
            SpanBuilder s5
        ) => SpanBuilder (s1, s2, s3, s4, s5) where
    sp (s1, s2, s3, s4, s5) = sp s1 <> sp s2 <> sp s3 <> sp s4 <> sp s5

instance MaySpanBuilder (Maybe Spanned.Span) where
    maySp ms = ms

instance MaySpanBuilder s => MaySpanBuilder [s] where
    maySp l = ofoldl'
        do \ms1 ms2 -> maySp ms1 <> maySp ms2
        Nothing
        l

instance (MaySpanBuilder s1, MaySpanBuilder s2)
        => MaySpanBuilder (s1, s2) where
    maySp (ms1, ms2) = maySp ms1 <> maySp ms2

instance (MaySpanBuilder s1, MaySpanBuilder s2, MaySpanBuilder s3)
        => MaySpanBuilder (s1, s2, s3) where
    maySp (ms1, ms2, ms3) = maySp ms1 <> maySp ms2 <> maySp ms3


data MaySideSpan s1 s2
    = s1 :<< s2
    | s2 :>> s1
    deriving (Eq, Show)

instance (SpanBuilder s1, MaySpanBuilder s2) => SpanBuilder (MaySideSpan s1 s2) where
    sp = \case
        s1 :<< ms2 -> case maySp ms2 of
            Nothing -> sp s1
            Just s2 -> sp (s1, s2)
        ms1 :>> s2 -> case maySp ms1 of
            Nothing -> sp s2
            Just s1 -> sp (s1, s2)


data AstParsed

type T = AstParsed

type instance Ast.XProgram AstParsed = Maybe Spanned.Span
type instance Ast.XDeclTypeSig AstParsed = Spanned.Span
type instance Ast.XDeclValSig AstParsed = Spanned.Span
type instance Ast.XDeclConSig AstParsed = Spanned.Span
type instance Ast.XDeclType AstParsed = Spanned.Span
type instance Ast.XDeclDataType AstParsed = Spanned.Span
type instance Ast.XDeclAlgDataType AstParsed = Spanned.Span
type instance Ast.XDeclNewType AstParsed = Spanned.Span
type instance Ast.XDeclVal AstParsed = Spanned.Span
type instance Ast.XDeclValBind AstParsed = Spanned.Span
type instance Ast.XDeclAppType AstParsed = Spanned.Span
type instance Ast.XDeclInfixType AstParsed = Spanned.Span
type instance Ast.XConAppType AstParsed = Spanned.Span
type instance Ast.XConInfixType AstParsed = Spanned.Span
type instance Ast.XDeclAppExpr AstParsed = Spanned.Span
type instance Ast.XDeclInfixExpr AstParsed = Spanned.Span
type instance Ast.XAppExpr AstParsed = Spanned.Span
type instance Ast.XUnivAppExpr AstParsed = Spanned.Span
type instance Ast.XBindVar AstParsed = Spanned.Span
type instance Ast.XUnivBindVar AstParsed = Spanned.Span
type instance Ast.XLitRational AstParsed = Spanned.Span
type instance Ast.XLitInteger AstParsed = Spanned.Span
type instance Ast.XLitByteString AstParsed = Spanned.Span
type instance Ast.XLitString AstParsed = Spanned.Span
type instance Ast.XLitByteChar AstParsed = Spanned.Span
type instance Ast.XLitChar AstParsed = Spanned.Span
type instance Ast.XTypeForall AstParsed = Spanned.Span
type instance Ast.XTypeInfix AstParsed = Spanned.Span
type instance Ast.XTypeApp AstParsed = Spanned.Span
type instance Ast.XTypeSig AstParsed = Spanned.Span
type instance Ast.XTypeCon AstParsed = Spanned.Span
type instance Ast.XTypeVar AstParsed = Spanned.Span
type instance Ast.XTypeLit AstParsed = Spanned.Span
type instance Ast.XTypeTuple AstParsed = Spanned.Span
type instance Ast.XTypeArray AstParsed = Spanned.Span
type instance Ast.XTypeRecord AstParsed = Spanned.Span
type instance Ast.XTypeAnn AstParsed = Spanned.Span
type instance Ast.XAppType AstParsed = Spanned.Span
type instance Ast.XUnivAppType AstParsed = Spanned.Span
type instance Ast.XExprSig AstParsed = Spanned.Span
type instance Ast.XExprInfix AstParsed = Spanned.Span
type instance Ast.XExprLambda AstParsed = Spanned.Span
type instance Ast.XExprLetrec AstParsed = Spanned.Span
type instance Ast.XExprLet AstParsed = Spanned.Span
type instance Ast.XExprMatch AstParsed = Spanned.Span
type instance Ast.XExprApp AstParsed = Spanned.Span
type instance Ast.XExprDo AstParsed = Spanned.Span
type instance Ast.XExprCon AstParsed = Spanned.Span
type instance Ast.XExprVar AstParsed = Spanned.Span
type instance Ast.XExprLit AstParsed = Spanned.Span
type instance Ast.XExprInterpString AstParsed = Spanned.Span
type instance Ast.XExprTuple AstParsed = Spanned.Span
type instance Ast.XExprArray AstParsed = Spanned.Span
type instance Ast.XExprRecord AstParsed = Spanned.Span
type instance Ast.XExprAnn AstParsed = Spanned.Span
type instance Ast.XInterpStringLit AstParsed = Spanned.Span
type instance Ast.XInterpStringExpr AstParsed = Spanned.Span
type instance Ast.XCaseAlt AstParsed = Spanned.Span
type instance Ast.XGuardedAlt AstParsed = Spanned.Span
type instance Ast.XDoStmtBind AstParsed = Spanned.Span
type instance Ast.XDoStmtMonBind AstParsed = Spanned.Span
type instance Ast.XDoStmtLetrec AstParsed = Spanned.Span
type instance Ast.XPatSig AstParsed = Spanned.Span
type instance Ast.XPatOr AstParsed = Spanned.Span
type instance Ast.XPatInfix AstParsed = Spanned.Span
type instance Ast.XPatApp AstParsed = Spanned.Span
type instance Ast.XPatUnivApp AstParsed = Spanned.Span
type instance Ast.XPatCon AstParsed = Spanned.Span
type instance Ast.XPatVar AstParsed = Spanned.Span
type instance Ast.XPatWildcard AstParsed = Spanned.Span
type instance Ast.XPatLit AstParsed = Spanned.Span
type instance Ast.XPatTuple AstParsed = Spanned.Span
type instance Ast.XPatArray AstParsed = Spanned.Span
type instance Ast.XPatRecord AstParsed = Spanned.Span
type instance Ast.XPatAnn AstParsed = Spanned.Span
type instance Ast.XPatOpConApp AstParsed = Spanned.Span
type instance Ast.XAppPat AstParsed = Spanned.Span
type instance Ast.XUnivAppPat AstParsed = Spanned.Span
type instance Ast.XPatRecordItem AstParsed = Spanned.Span
type instance Ast.XTypeRecordItem AstParsed = Spanned.Span
type instance Ast.XExprRecordItem AstParsed = Spanned.Span

instance Ast.XEq AstParsed
instance Ast.XShow AstParsed

instance LiftType.LiftType AstParsed where
    liftType _ = [t|AstParsed|]

instance MaySpanBuilder (Ast.Program AstParsed) where
    maySp = \case
        Ast.Program _ x -> x

instance SpanBuilder (Ast.Decl AstParsed) where
    sp = \case
        Ast.DeclTypeSig _ _ x       -> x
        Ast.DeclValSig _ _ x        -> x
        Ast.DeclConSig _ _ x        -> x
        Ast.DeclType _ _ _ x        -> x
        Ast.DeclDataType _ _ _ x    -> x
        Ast.DeclAlgDataType _ _ _ x -> x
        Ast.DeclNewType _ _ _ x     -> x
        Ast.DeclVal _ _ _ x         -> x
        Ast.DeclValBind _ _ _ x     -> x

instance SpanBuilder (Ast.DeclType AstParsed) where
    sp = \case
        Ast.DeclAppType _ _ _ x     -> x
        Ast.DeclInfixType _ _ _ _ x -> x

instance SpanBuilder (Ast.ConType AstParsed) where
    sp = \case
        Ast.ConAppType _ _ x     -> x
        Ast.ConInfixType _ _ _ x -> x

instance SpanBuilder (Ast.DeclExpr AstParsed) where
    sp = \case
        Ast.DeclAppExpr _ _ _ x     -> x
        Ast.DeclInfixExpr _ _ _ _ x -> x

instance SpanBuilder (Ast.TypeExpr AstParsed) where
    sp = \case
        Ast.TypeForall _ _ x  -> x
        Ast.TypeInfix _ _ _ x -> x
        Ast.TypeApp _ _ x     -> x
        Ast.TypeSig _ _ x     -> x
        Ast.TypeCon _ x       -> x
        Ast.TypeVar _ x       -> x
        Ast.TypeLit _ x       -> x
        Ast.TypeTuple _ x     -> x
        Ast.TypeArray _ x     -> x
        Ast.TypeRecord _ x    -> x
        Ast.TypeAnn _ x       -> x

instance SpanBuilder (Ast.AppType AstParsed) where
    sp = \case
        Ast.AppType _ x     -> x
        Ast.UnivAppType _ x -> x

instance SpanBuilder (Ast.TypeRecordItem AstParsed) where
    sp = \case
        Ast.TypeRecordItem _ _ x -> x

instance SpanBuilder (Ast.Expr AstParsed) where
    sp = \case
        Ast.ExprSig _ _ x        -> x
        Ast.ExprInfix _ _ _ x    -> x
        Ast.ExprApp _ _ x        -> x
        Ast.ExprLambda _ x       -> x
        Ast.ExprMatch _ _ x      -> x
        Ast.ExprLetrec _ _ x     -> x
        Ast.ExprLet _ _ x        -> x
        Ast.ExprDo _ _ x         -> x
        Ast.ExprCon _ x          -> x
        Ast.ExprVar _ x          -> x
        Ast.ExprLit _ x          -> x
        Ast.ExprInterpString _ x -> x
        Ast.ExprTuple _ x        -> x
        Ast.ExprArray _ x        -> x
        Ast.ExprRecord _ x       -> x
        Ast.ExprAnn _ x          -> x

instance SpanBuilder (Ast.AppExpr AstParsed) where
    sp = \case
        Ast.AppExpr _ x     -> x
        Ast.UnivAppExpr _ x -> x

instance SpanBuilder (Ast.CaseAlt AstParsed) where
    sp = \case
        Ast.CaseAlt _ _ x -> x

instance SpanBuilder (Ast.GuardedAlt AstParsed) where
    sp = \case
        Ast.GuardedAlt _ _ x -> x

instance SpanBuilder (Ast.DoStmt AstParsed) where
    sp = \case
        Ast.DoStmtBind _ _ _ x    -> x
        Ast.DoStmtMonBind _ _ _ x -> x
        Ast.DoStmtLetrec _ x      -> x

instance SpanBuilder (Ast.ExprRecordItem AstParsed) where
    sp = \case
        Ast.ExprRecordItem _ _ x -> x

instance SpanBuilder (Ast.Pat AstParsed) where
    sp = \case
        Ast.PatSig _ _ x     -> x
        Ast.PatOr _ x        -> x
        Ast.PatInfix _ _ _ x -> x
        Ast.PatConApp _ _ x  -> x
        Ast.PatVar _ x       -> x
        Ast.PatWildcard _ x  -> x
        Ast.PatLit _ x       -> x
        Ast.PatTuple _ x     -> x
        Ast.PatArray _ x     -> x
        Ast.PatRecord _ x    -> x
        Ast.PatUnivApp _ _ x -> x
        Ast.PatAnn _ x       -> x

instance SpanBuilder (Ast.PatOp AstParsed) where
    sp = \case
        Ast.PatOpConApp _ _ x -> x

instance SpanBuilder (Ast.AppPat AstParsed) where
    sp = \case
        Ast.AppPat _ x     -> x
        Ast.UnivAppPat _ x -> x

instance SpanBuilder (Ast.PatRecordItem AstParsed) where
    sp = \case
        Ast.PatRecordItem _ _ x -> x

instance SpanBuilder (Ast.Lit AstParsed) where
    sp = \case
        Ast.LitRational _ x   -> x
        Ast.LitInteger _ x    -> x
        Ast.LitByteString _ x -> x
        Ast.LitString _ x     -> x
        Ast.LitByteChar _ x   -> x
        Ast.LitChar _ x       -> x

instance SpanBuilder (Ast.InterpStringPart AstParsed) where
    sp = \case
        Ast.InterpStringLit _ x  -> x
        Ast.InterpStringExpr _ x -> x

instance SpanBuilder (Ast.BindVar AstParsed) where
    sp = \case
        Ast.BindVar _ _ x     -> x
        Ast.UnivBindVar _ _ x -> x
