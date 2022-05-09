module Language.Quell.Pipeline.Source2Ast.OrdinarySampleSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Pipeline.Source2Ast

import qualified Conduit
import qualified Data.ByteString.Char8 as Char8
import qualified Language.Quell.Pipeline.Source2Ast.TestRunner as TestRunner
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer as Lexer
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Type.TextId as TextId


sampleSource :: Lexer.LexerMonad s m => Source () m
sampleSource = Source
    { sourceEncoding = Encoding.EncodingUtf8
    , sourceConduit = Conduit.yieldMany
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
spec = do
    describe "source2Tokens" do
        it "should convert the sample source" do
            let (reports, tokens) = TestRunner.runTestRunner
                    do Conduit.sourceToList
                        do source2Tokens sampleSource
            otoList reports `shouldBe` []
            tokens `shouldBe`
                [ Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 0, locLine = 0, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 5, locLine = 0, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.KwType
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 6, locLine = 0, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 7, locLine = 0, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "T"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 7, locLine = 0, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 8, locLine = 0, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 9, locLine = 0, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 13, locLine = 0, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Type"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 14, locLine = 0, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 16, locLine = 0, locCol = 16}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 17, locLine = 0, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 21, locLine = 0, locCol = 21}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Type"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 22, locLine = 1, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 27, locLine = 1, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.KwType
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 28, locLine = 1, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 29, locLine = 1, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "T"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 30, locLine = 1, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 31, locLine = 1, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 32, locLine = 1, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 33, locLine = 1, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 34, locLine = 1, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 35, locLine = 1, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 35, locLine = 1, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 36, locLine = 1, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 36, locLine = 1, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 37, locLine = 1, locCol = 15}
                        }
                    , unSpanned = Token.TokLexeme Token.SpComma
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 38, locLine = 1, locCol = 16}
                        , endLoc = Spanned.Loc {locBytesPos = 39, locLine = 1, locCol = 17}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 39, locLine = 1, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 40, locLine = 1, locCol = 18}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 42, locLine = 3, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 47, locLine = 3, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.KwData
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 48, locLine = 3, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 49, locLine = 3, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 50, locLine = 3, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 51, locLine = 3, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 52, locLine = 3, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 53, locLine = 3, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 56, locLine = 4, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 57, locLine = 4, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme Token.SymOr
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 58, locLine = 4, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 60, locLine = 4, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 61, locLine = 4, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 62, locLine = 4, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 62, locLine = 4, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 63, locLine = 4, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 64, locLine = 4, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 65, locLine = 4, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 65, locLine = 4, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 66, locLine = 4, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 67, locLine = 4, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 68, locLine = 4, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 71, locLine = 5, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 72, locLine = 5, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme Token.SymOr
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 73, locLine = 5, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 75, locLine = 5, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 76, locLine = 5, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 77, locLine = 5, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 78, locLine = 5, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 79, locLine = 5, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 79, locLine = 5, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 80, locLine = 5, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 81, locLine = 5, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 82, locLine = 5, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 82, locLine = 5, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 83, locLine = 5, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 86, locLine = 6, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 87, locLine = 6, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme Token.SymOr
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 88, locLine = 6, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 90, locLine = 6, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D3"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 91, locLine = 6, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 92, locLine = 6, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 92, locLine = 6, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 94, locLine = 6, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "IA"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 95, locLine = 6, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 96, locLine = 6, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 96, locLine = 6, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 97, locLine = 6, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 100, locLine = 7, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 106, locLine = 7, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme Token.KwWhere
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 113, locLine = 8, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 118, locLine = 8, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme Token.KwType
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 119, locLine = 8, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 121, locLine = 8, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "IA"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 122, locLine = 8, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 123, locLine = 8, locCol = 16}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 124, locLine = 8, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 125, locLine = 8, locCol = 18}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 126, locLine = 8, locCol = 19}
                        , endLoc = Spanned.Loc {locBytesPos = 129, locLine = 8, locCol = 22}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 130, locLine = 8, locCol = 23}
                        , endLoc = Spanned.Loc {locBytesPos = 132, locLine = 8, locCol = 25}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 133, locLine = 8, locCol = 26}
                        , endLoc = Spanned.Loc {locBytesPos = 134, locLine = 8, locCol = 27}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 136, locLine = 10, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 141, locLine = 10, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.KwData
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 142, locLine = 10, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 143, locLine = 10, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 144, locLine = 10, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 145, locLine = 10, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 146, locLine = 10, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 150, locLine = 10, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Type"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 151, locLine = 10, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 153, locLine = 10, locCol = 17}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 154, locLine = 10, locCol = 18}
                        , endLoc = Spanned.Loc {locBytesPos = 158, locLine = 10, locCol = 22}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Type"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 159, locLine = 10, locCol = 23}
                        , endLoc = Spanned.Loc {locBytesPos = 165, locLine = 10, locCol = 29}
                        }
                    , unSpanned = Token.TokLexeme Token.KwWhere
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 168, locLine = 11, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 170, locLine = 11, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 170, locLine = 11, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 171, locLine = 11, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 172, locLine = 11, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 173, locLine = 11, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 174, locLine = 11, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 175, locLine = 11, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 176, locLine = 11, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 178, locLine = 11, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 179, locLine = 11, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 180, locLine = 11, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 181, locLine = 11, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 183, locLine = 11, locCol = 17}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 184, locLine = 11, locCol = 18}
                        , endLoc = Spanned.Loc {locBytesPos = 185, locLine = 11, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 186, locLine = 11, locCol = 20}
                        , endLoc = Spanned.Loc {locBytesPos = 187, locLine = 11, locCol = 21}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 190, locLine = 12, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 192, locLine = 12, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 192, locLine = 12, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 193, locLine = 12, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 194, locLine = 12, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 195, locLine = 12, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 196, locLine = 12, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 198, locLine = 12, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 199, locLine = 12, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 200, locLine = 12, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "D"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 201, locLine = 12, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 202, locLine = 12, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 203, locLine = 12, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 205, locLine = 12, locCol = 17}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 206, locLine = 12, locCol = 18}
                        , endLoc = Spanned.Loc {locBytesPos = 207, locLine = 12, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 208, locLine = 12, locCol = 20}
                        , endLoc = Spanned.Loc {locBytesPos = 209, locLine = 12, locCol = 21}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 212, locLine = 13, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 214, locLine = 13, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R3"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 214, locLine = 13, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 215, locLine = 13, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 216, locLine = 13, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 218, locLine = 13, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "IA"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 219, locLine = 13, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 220, locLine = 13, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 221, locLine = 13, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 223, locLine = 13, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 224, locLine = 13, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 225, locLine = 13, locCol = 15}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "R"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 226, locLine = 13, locCol = 16}
                        , endLoc = Spanned.Loc {locBytesPos = 227, locLine = 13, locCol = 17}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 231, locLine = 15, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 236, locLine = 15, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme Token.KwType
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 237, locLine = 15, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 239, locLine = 15, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "IA"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 240, locLine = 15, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 241, locLine = 15, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 242, locLine = 15, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 243, locLine = 15, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 244, locLine = 15, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 247, locLine = 15, locCol = 18}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 248, locLine = 15, locCol = 19}
                        , endLoc = Spanned.Loc {locBytesPos = 250, locLine = 15, locCol = 21}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 251, locLine = 15, locCol = 22}
                        , endLoc = Spanned.Loc {locBytesPos = 252, locLine = 15, locCol = 23}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 254, locLine = 17, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 255, locLine = 17, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "f"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 255, locLine = 17, locCol = 1}
                        , endLoc = Spanned.Loc {locBytesPos = 256, locLine = 17, locCol = 2}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 257, locLine = 17, locCol = 3}
                        , endLoc = Spanned.Loc {locBytesPos = 260, locLine = 17, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 261, locLine = 17, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 263, locLine = 17, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 264, locLine = 17, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 269, locLine = 17, locCol = 15}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Maybe"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 270, locLine = 17, locCol = 16}
                        , endLoc = Spanned.Loc {locBytesPos = 273, locLine = 17, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 274, locLine = 17, locCol = 20}
                        , endLoc = Spanned.Loc {locBytesPos = 276, locLine = 17, locCol = 22}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "->"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 277, locLine = 17, locCol = 23}
                        , endLoc = Spanned.Loc {locBytesPos = 280, locLine = 17, locCol = 26}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 281, locLine = 17, locCol = 27}
                        , endLoc = Spanned.Loc {locBytesPos = 282, locLine = 17, locCol = 28}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 283, locLine = 17, locCol = 29}
                        , endLoc = Spanned.Loc {locBytesPos = 288, locLine = 17, locCol = 34}
                        }
                    , unSpanned = Token.TokLexeme Token.KwCase
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 291, locLine = 18, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 292, locLine = 18, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 0)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 292, locLine = 18, locCol = 3}
                        , endLoc = Spanned.Loc {locBytesPos = 293, locLine = 18, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme Token.SpComma
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 294, locLine = 18, locCol = 5}
                        , endLoc = Spanned.Loc {locBytesPos = 298, locLine = 18, locCol = 9}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "None"))}
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 299, locLine = 18, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 301, locLine = 18, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme Token.SymThen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 302, locLine = 18, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 303, locLine = 18, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 0)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 306, locLine = 19, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 308, locLine = 19, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 308, locLine = 19, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 309, locLine = 19, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme Token.SpComma
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 310, locLine = 19, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 314, locLine = 19, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Some"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 315, locLine = 19, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 317, locLine = 19, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 318, locLine = 19, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 323, locLine = 19, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme Token.KwWhen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 330, locLine = 20, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 332, locLine = 20, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 333, locLine = 20, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 335, locLine = 20, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "=="))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 336, locLine = 20, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 337, locLine = 20, locCol = 13}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 0)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 338, locLine = 20, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 340, locLine = 20, locCol = 16}
                        }
                    , unSpanned = Token.TokLexeme Token.SymThen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 341, locLine = 20, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 343, locLine = 20, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit  "i1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 350, locLine = 21, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 354, locLine = 21, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit  "else"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 358, locLine = 21, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 360, locLine = 21, locCol = 16}
                        }
                    , unSpanned = Token.TokLexeme Token.SymThen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 361, locLine = 21, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 363, locLine = 21, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 364, locLine = 21, locCol = 20}
                        , endLoc = Spanned.Loc {locBytesPos = 365, locLine = 21, locCol = 21}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "+"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 366, locLine = 21, locCol = 22}
                        , endLoc = Spanned.Loc {locBytesPos = 368, locLine = 21, locCol = 24}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 370, locLine = 23, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 371, locLine = 23, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 371, locLine = 23, locCol = 1}
                        , endLoc = Spanned.Loc {locBytesPos = 372, locLine = 23, locCol = 2}
                        }
                    , unSpanned = Token.TokLexeme Token.SymColon
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 373, locLine = 23, locCol = 3}
                        , endLoc = Spanned.Loc {locBytesPos = 378, locLine = 23, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Maybe"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 379, locLine = 23, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 382, locLine = 23, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Int"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 383, locLine = 24, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 384, locLine = 24, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 385, locLine = 24, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 386, locLine = 24, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 387, locLine = 24, locCol = 4}
                        , endLoc = Spanned.Loc {locBytesPos = 390, locLine = 24, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme Token.KwDo
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 393, locLine = 25, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 395, locLine = 25, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 396, locLine = 25, locCol = 5}
                        , endLoc = Spanned.Loc {locBytesPos = 398, locLine = 25, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme Token.SymBind
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 399, locLine = 25, locCol = 8}
                        , endLoc = Spanned.Loc {locBytesPos = 403, locLine = 25, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdConId (TextId.stringLit "Some"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 404, locLine = 25, locCol = 13}
                        , endLoc = Spanned.Loc {locBytesPos = 405, locLine = 25, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 0)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 408, locLine = 26, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 410, locLine = 26, locCol = 4}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 411, locLine = 26, locCol = 5}
                        , endLoc = Spanned.Loc {locBytesPos = 412, locLine = 26, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 413, locLine = 26, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 417, locLine = 26, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme Token.KwLet
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 418, locLine = 26, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 420, locLine = 26, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i3"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 421, locLine = 26, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 422, locLine = 26, locCol = 16}
                        }
                    , unSpanned = Token.TokLexeme Token.SymEqual
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 423, locLine = 26, locCol = 17}
                        , endLoc = Spanned.Loc {locBytesPos = 424, locLine = 26, locCol = 18}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 1)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 425, locLine = 26, locCol = 19}
                        , endLoc = Spanned.Loc {locBytesPos = 428, locLine = 26, locCol = 22}
                        }
                    , unSpanned = Token.TokLexeme Token.KwIn
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 429, locLine = 26, locCol = 23}
                        , endLoc = Spanned.Loc {locBytesPos = 431, locLine = 26, locCol = 25}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i1"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 432, locLine = 26, locCol = 26}
                        , endLoc = Spanned.Loc {locBytesPos = 433, locLine = 26, locCol = 27}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarSym (TextId.stringLit "+"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 434, locLine = 26, locCol = 28}
                        , endLoc = Spanned.Loc {locBytesPos = 436, locLine = 26, locCol = 30}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 439, locLine = 27, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 445, locLine = 27, locCol = 8}
                        }
                    , unSpanned = Token.TokLexeme Token.KwYield
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 446, locLine = 27, locCol = 9}
                        , endLoc = Spanned.Loc {locBytesPos = 448, locLine = 27, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "i2"))
                    }
                ]
