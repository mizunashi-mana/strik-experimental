module Language.Quell.Parsing.Parser.AstParsed (
    SpannedBuilder (..),
    AstParsed,
) where

import Language.Quell.Prelude

import qualified Language.Quell.Parsing.Spanned         as Spanned
import qualified Language.Quell.Type.Ast                as Ast


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

instance SpannedBuilder (Ast.TypeSigDecl AstParsed) where
    sp = \case
        Ast.TypeSigDecl _ _ x -> x

instance SpannedBuilder (Ast.ValSigDecl AstParsed) where
    sp = \case
        Ast.ValSigDecl _ _ x -> x

instance SpannedBuilder (Ast.ConSigDecl AstParsed) where
    sp = \case
        Ast.ConSigDecl _ _ x -> x

instance SpannedBuilder (Ast.TypeExpr AstParsed) where
    sp = \case
        Ast.TypeForall _ _ x -> x
        Ast.TypeInfix _ _ _ x -> x
        Ast.TypeApp _ _ x -> x
        Ast.TypeSig _ _ x -> x
        Ast.TypeCon _ x -> x
        Ast.TypeVar _ x -> x
        Ast.TypeLit _ x -> x
        Ast.TypeTuple _ x -> x
        Ast.TypeArray _ x -> x
        Ast.TypeRecord _ x -> x
        Ast.TypeAnn _ x -> x

instance SpannedBuilder (Ast.AppType AstParsed) where
    sp = \case
        Ast.AppType _ x -> x
        Ast.UnivAppType _ x -> x

instance SpannedBuilder (Ast.Expr AstParsed) where
    sp = \case
        Ast.ExprSig _ _ x -> x
        Ast.ExprInfix _ _ _ x -> x
        Ast.ExprApp _ _ x -> x
        Ast.ExprLambda _ x -> x
        Ast.ExprCase _ _ x -> x
        Ast.ExprLetrec _ _ x -> x
        Ast.ExprLet _ _ x -> x
        Ast.ExprDo _ _ x -> x
        Ast.ExprCon _ x -> x
        Ast.ExprVar _ x -> x
        Ast.ExprLit _ x -> x
        Ast.ExprInterpString _ x -> x
        Ast.ExprTuple _ x -> x
        Ast.ExprArray _ x -> x
        Ast.ExprRecord _ x -> x
        Ast.ExprAnn _ x -> x

instance SpannedBuilder (Ast.AppExpr AstParsed) where
    sp = \case
        Ast.AppExpr _ x -> x
        Ast.UnivAppExpr _ x -> x

instance SpannedBuilder (Ast.Lit AstParsed) where
    sp = \case
        Ast.LitRational _ x -> x
        Ast.LitInteger _ x -> x
        Ast.LitByteString _ x -> x
        Ast.LitString _ x -> x
        Ast.LitByteChar _ x -> x
        Ast.LitChar _ x -> x

instance SpannedBuilder (Ast.InterpStringPart AstParsed) where
    sp = \case
        Ast.InterpStringLit _ x -> x
        Ast.InterpStringExpr _ x -> x

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


data AstParsed

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
type instance Ast.XTypeSigDecl AstParsed = Spanned.Span
type instance Ast.XValSigDecl AstParsed = Spanned.Span
type instance Ast.XConSigDecl AstParsed = Spanned.Span
type instance Ast.XTypeDecl AstParsed = Spanned.Span
type instance Ast.XValBind AstParsed = Spanned.Span
type instance Ast.XMonBind AstParsed = Spanned.Span
type instance Ast.XDeclTypeSig AstParsed = Spanned.Span
type instance Ast.XDeclValSig AstParsed = Spanned.Span
type instance Ast.XDeclConSig AstParsed = Spanned.Span
type instance Ast.XDeclType AstParsed = Spanned.Span
type instance Ast.XDeclDataType AstParsed = Spanned.Span
type instance Ast.XDeclVal AstParsed = Spanned.Span
type instance Ast.XDeclValBind AstParsed = Spanned.Span
type instance Ast.XDeclMonBind AstParsed = Spanned.Span
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

instance Ast.XEq AstParsed
instance Ast.XShow AstParsed
