{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Parser.LiftTypes where

import           Language.Strik.Prelude

import qualified Language.Parser.Ptera.TH               as Ptera
import           Language.Strik.Frontend.Data.ParsedAst
import           Language.Strik.Frontend.Data.Token
import           Language.Strik.Parsing.Parser.Layout

instance Ptera.LiftType Token where
    liftType _ = [t|Token|]

instance Ptera.LiftType TokenWithL where
    liftType _ = [t|TokenWithL|]

instance Ptera.LiftType tag => Ptera.LiftType (Program tag) where
    liftType _ = [t|Program $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType tag => Ptera.LiftType (Expr tag) where
    liftType _ = [t|Expr $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType tag => Ptera.LiftType (TypeExpr tag) where
    liftType _ = [t|TypeExpr $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType tag => Ptera.LiftType (Decl tag) where
    liftType _ = [t|Decl $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType tag => Ptera.LiftType (LocalDecl tag) where
    liftType _ = [t|LocalDecl $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType tag => Ptera.LiftType (LetItem tag) where
    liftType _ = [t|LetItem $(Ptera.liftType do Proxy @tag)|]

instance Ptera.LiftType a => Ptera.LiftType (Bundle a) where
    liftType _ = [t|Bundle $(Ptera.liftType do Proxy @a)|]
