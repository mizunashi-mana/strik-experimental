module Language.Quell.Parsing.Parser.AstParsedTag where

import qualified Language.Quell.Frontend.Data.ParsedAst as ParsedAst
import qualified Language.Quell.Parsing.Spanned as Spanned

type T = AstParsedTag

type AstParsedTag = ParsedAst.Bundle Spanned.Span
