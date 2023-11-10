module Language.Strik.Parsing.Parser.AstParsedTag where

import Language.Strik.Prelude

import qualified Language.Strik.Frontend.Data.ParsedAst as ParsedAst
import qualified Language.Strik.Parsing.Spanned as Spanned
import qualified Language.Strik.Parsing.Parser.Layout as Layout


type T = AstParsedTag

type AstParsedTag = ParsedAst.Bundle Spanned.Span


tokenSpan :: Layout.TokenWithL -> Maybe Spanned.Span
tokenSpan = \case
    Layout.TokenRaw spanned ->
        Just do Spanned.getSpan spanned
    Layout.TokenVirt{} ->
        Nothing
    Layout.LayoutError{} ->
        Nothing


(<>>) :: Maybe Spanned.Span -> Spanned.Span -> Spanned.Span
maySp1 <>> sp2 = case maySp1 of
    Nothing ->
        sp2
    Just sp1 ->
        sp1 <> sp2

(<<>) :: Spanned.Span -> Maybe Spanned.Span -> Spanned.Span
sp1 <<> maySp2 = case maySp2 of
    Nothing ->
        sp1
    Just sp2 ->
        sp1 <> sp2
