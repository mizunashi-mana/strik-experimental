module Language.Quell.Parsing.Parser.AstParsed (
    SpannedBuilder (..),
    MayOneSideSpan (..),
    maySp,
    AstParsed,
    T,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Ast        as Ast


class SpannedBuilder s where
    sp :: s -> Spanned.Span

    spn :: s -> a -> Spanned.Spanned a
    spn s x = Spanned.Spanned
        {
            getSpan = sp s,
            unSpanned = x
        }

    spAnn :: s -> (Spanned.Span -> a) -> a
    spAnn s f = f do sp s

instance SpannedBuilder Spanned.Span where
    sp s = s

instance SpannedBuilder (Spanned.Spanned a) where
    sp sx = Spanned.getSpan sx

instance SpannedBuilder (Ast.TypeExpr AstParsed) where
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

instance SpannedBuilder (Ast.AppType AstParsed) where
    sp = \case
        Ast.AppType _ x     -> x
        Ast.UnivAppType _ x -> x

instance SpannedBuilder (Ast.Expr AstParsed) where
    sp = \case
        Ast.ExprSig _ _ x        -> x
        Ast.ExprInfix _ _ _ x    -> x
        Ast.ExprApp _ _ x        -> x
        Ast.ExprLambda _ x       -> x
        Ast.ExprCase _ _ x       -> x
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

instance SpannedBuilder (Ast.AppExpr AstParsed) where
    sp = \case
        Ast.AppExpr _ x     -> x
        Ast.UnivAppExpr _ x -> x

instance SpannedBuilder (Ast.DeclExpr AstParsed) where
    sp = \case
        Ast.DeclAppExpr _ _ x     -> x
        Ast.DeclInfixExpr _ _ _ x -> x

instance SpannedBuilder (Ast.DoStmt AstParsed) where
    sp = \case
        Ast.DoStmtBind _ _ _ x    -> x
        Ast.DoStmtMonBind _ _ _ x -> x
        Ast.DoStmtLetrec _ x      -> x

instance SpannedBuilder (Ast.Pat AstParsed) where
    sp = \case
        Ast.PatSig _ _ x     -> x
        Ast.PatOr _ x        -> x
        Ast.PatInfix _ _ _ x -> x
        Ast.PatApp _ _ x     -> x
        Ast.PatCon _ x       -> x
        Ast.PatVar _ x       -> x
        Ast.PatWildcard _ x  -> x
        Ast.PatLit _ x       -> x
        Ast.PatTuple _ x     -> x
        Ast.PatArray _ x     -> x
        Ast.PatRecord _ x    -> x
        Ast.PatUnivApp _ _ x -> x
        Ast.PatAnn _ x       -> x

instance SpannedBuilder (Ast.Lit AstParsed) where
    sp = \case
        Ast.LitRational _ x   -> x
        Ast.LitInteger _ x    -> x
        Ast.LitByteString _ x -> x
        Ast.LitString _ x     -> x
        Ast.LitByteChar _ x   -> x
        Ast.LitChar _ x       -> x

instance SpannedBuilder (Ast.InterpStringPart AstParsed) where
    sp = \case
        Ast.InterpStringLit _ x  -> x
        Ast.InterpStringExpr _ x -> x

instance SpannedBuilder (Ast.BindVar AstParsed) where
    sp = \case
        Ast.BindVar _ _ x     -> x
        Ast.UnivBindVar _ _ x -> x

instance SpannedBuilder (Ast.AppPat AstParsed) where
    sp = \case
        Ast.AppPat _ x     -> x
        Ast.UnivAppPat _ x -> x

instance SpannedBuilder (Ast.Decl AstParsed) where
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

instance SpannedBuilder (Ast.DeclType AstParsed) where
    sp = \case
        Ast.DeclAppType _ _ x     -> x
        Ast.DeclInfixType _ _ _ x -> x

instance SpannedBuilder (Ast.ImplType AstParsed) where
    sp = \case
        Ast.ImplAppType _ _ x     -> x
        Ast.ImplInfixType _ _ _ x -> x

instance SpannedBuilder (Ast.CaseAlt AstParsed) where
    sp = \case
        Ast.CaseAlt _ _ x -> x

instance SpannedBuilder (Ast.GuardedAlt AstParsed) where
    sp = \case
        Ast.GuardedAlt _ _ x -> x

instance SpannedBuilder s => SpannedBuilder (NonEmpty s) where
    sp l = sconcat do l <&> \s -> sp s

instance (SpannedBuilder s1, SpannedBuilder s2) => SpannedBuilder (s1, s2) where
    sp (s1, s2) = sp s1 <> sp s2

instance (SpannedBuilder s1, SpannedBuilder s2, SpannedBuilder s3)
        => SpannedBuilder (s1, s2, s3) where
    sp (s1, s2, s3) = sp s1 <> sp s2 <> sp s3

instance (SpannedBuilder s1, SpannedBuilder s2, SpannedBuilder s3, SpannedBuilder s4)
        => SpannedBuilder (s1, s2, s3, s4) where
    sp (s1, s2, s3, s4) = sp s1 <> sp s2 <> sp s3 <> sp s4

instance (
            SpannedBuilder s1,
            SpannedBuilder s2,
            SpannedBuilder s3,
            SpannedBuilder s4,
            SpannedBuilder s5
        ) => SpannedBuilder (s1, s2, s3, s4, s5) where
    sp (s1, s2, s3, s4, s5) = sp s1 <> sp s2 <> sp s3 <> sp s4 <> sp s5

data MayOneSideSpan a b
    = a :> b
    | b :< a
    deriving (Eq, Show)

instance (SpannedBuilder s1, SpannedBuilder s2)
        => SpannedBuilder (MayOneSideSpan (Maybe s1) s2) where
    sp = \case
        Nothing :> s2      -> sp s2
        s1      :< Nothing -> sp s1
        Just s1 :> s2      -> sp (s1, s2)
        s1      :< Just s2 -> sp (s1, s2)

instance (SpannedBuilder s1, SpannedBuilder s2)
        => SpannedBuilder (MayOneSideSpan [s1] s2) where
    sp = \case
        []          :> s2         -> sp s2
        s1          :< []         -> sp s1
        (sx1:sxs1)  :> s2         -> sp (sx1 :| sxs1, s2)
        s1          :< (sx2:sxs2) -> sp (s1, sx2 :| sxs2)

maySp :: [Maybe Spanned.Span] -> Maybe Spanned.Span
maySp mss = ofoldl'
    do \ms1 ms2 -> ms1 <> ms2
    Nothing
    mss


data AstParsed

type T = AstParsed

type instance Ast.XProgram AstParsed = Maybe Spanned.Span
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
type instance Ast.XDeclTypeSig AstParsed = Spanned.Span
type instance Ast.XDeclValSig AstParsed = Spanned.Span
type instance Ast.XDeclConSig AstParsed = Spanned.Span
type instance Ast.XDeclType AstParsed = Spanned.Span
type instance Ast.XDeclDataType AstParsed = Spanned.Span
type instance Ast.XDeclAlgDataType AstParsed = Spanned.Span
type instance Ast.XDeclNewType AstParsed = Spanned.Span
type instance Ast.XDeclVal AstParsed = Spanned.Span
type instance Ast.XDeclValBind AstParsed = Spanned.Span
type instance Ast.XExprSig AstParsed = Spanned.Span
type instance Ast.XExprInfix AstParsed = Spanned.Span
type instance Ast.XExprLambda AstParsed = Spanned.Span
type instance Ast.XExprLetrec AstParsed = Spanned.Span
type instance Ast.XExprLet AstParsed = Spanned.Span
type instance Ast.XExprCase AstParsed = Spanned.Span
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
type instance Ast.XDeclAppType AstParsed = Spanned.Span
type instance Ast.XDeclInfixType AstParsed = Spanned.Span
type instance Ast.XImplAppType AstParsed = Spanned.Span
type instance Ast.XImplInfixType AstParsed = Spanned.Span
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
type instance Ast.XAppPat AstParsed = Spanned.Span
type instance Ast.XUnivAppPat AstParsed = Spanned.Span
type instance Ast.XDeclAppExpr AstParsed = Spanned.Span
type instance Ast.XDeclInfixExpr AstParsed = Spanned.Span

instance Ast.XEq AstParsed
instance Ast.XShow AstParsed
