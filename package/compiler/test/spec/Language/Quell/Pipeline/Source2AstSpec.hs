module Language.Quell.Pipeline.Source2AstSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Pipeline.Source2Ast

import qualified Conduit
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Data.ByteString.Char8 as Char8


sampleSource :: Source Conduit.Void Identity
sampleSource = Source
    { sourceConduit = Conduit.yieldMany
        [ Char8.pack "#type T: Type -> Type\n"
        , Char8.pack "#type T a = (a, a)\n"
        , Char8.pack "\n"
        , Char8.pack "#data D a =\n"
        , Char8.pack "  | D1 (D a) a\n"
        , Char8.pack "  | D2 a (D a)\n"
        , Char8.pack "  | D3 (IA a)\n"
        , Char8.pack "  #where\n"
        , Char8.pack "      #type IA a = Int -> a\n"
        , Char8.pack "\n"
        , Char8.pack "#data R : Type -> Type #where\n"
        , Char8.pack "  R1: D a -> a -> R a\n"
        , Char8.pack "  R2: a -> D a -> R a\n"
        , Char8.pack "  R3: IA a -> R a\n"
        , Char8.pack "\n"
        , Char8.pack "  #type IA a = Int -> a\n"
        , Char8.pack "\n"
        , Char8.pack "f: Int -> Maybe Int -> Int = #case\n"
        , Char8.pack "  0, None #> 0\n"
        , Char8.pack "  i1, Some i2 #when\n"
        , Char8.pack "      i2 == 0 #> i1\n"
        , Char8.pack "      else    #> i1 + i2\n"
        , Char8.pack "\n"
        , Char8.pack "i: Maybe Int\n"
        , Char8.pack "i = #do\n"
        , Char8.pack "  i1 #< Some 0\n"
        , Char8.pack "  i2 = #let i3 = 1 #in i1 + i2\n"
        , Char8.pack "  #yield i2\n"
        ]
    }

spec :: Spec
spec = pure ()
