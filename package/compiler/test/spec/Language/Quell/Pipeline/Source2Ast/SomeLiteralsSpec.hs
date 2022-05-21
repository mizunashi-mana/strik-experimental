module Language.Quell.Pipeline.Source2Ast.SomeLiteralsSpec where

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
import qualified Language.Quell.Parsing.Parser.Layout  as Layout
import qualified Language.Quell.Parsing.Parser.Layout.TokenWithLForTests as TokenWithLForTests


sampleSource :: Lexer.LexerMonad s m => Source () m
sampleSource = Source
    { sourceEncoding = Encoding.EncodingUtf8
    , sourceConduit = Conduit.yieldMany
        [ Char8.pack "[id\n"
        , Char8.pack "x(0b0)aa00,1\n"
        , Char8.pack "01 +103-40.0a0,40e10o0]\n"
        , Char8.pack "(#r\"a\\x00\" '\\u{11}')\n"
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
                        , endLoc = Spanned.Loc {locBytesPos = 1, locLine = 0, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme Token.SpBrackOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 1, locLine = 0, locCol = 1}
                        , endLoc = Spanned.Loc {locBytesPos = 3, locLine = 0, locCol = 3}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "id"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 4, locLine = 1, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 5, locLine = 1, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "x"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 5, locLine = 1, locCol = 1}
                        , endLoc = Spanned.Loc {locBytesPos = 6, locLine = 1, locCol = 2}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 6, locLine = 1, locCol = 2}
                        , endLoc = Spanned.Loc {locBytesPos = 9, locLine = 1, locCol = 5}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 0)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 9, locLine = 1, locCol = 5}
                        , endLoc = Spanned.Loc {locBytesPos = 10, locLine = 1, locCol = 6}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 10, locLine = 1, locCol = 6}
                        , endLoc = Spanned.Loc {locBytesPos = 14, locLine = 1, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "aa00"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 14, locLine = 1, locCol = 10}
                        , endLoc = Spanned.Loc {locBytesPos = 15, locLine = 1, locCol = 11}
                        }
                    , unSpanned = Token.TokLexeme Token.SpComma
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 15, locLine = 1, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 16, locLine = 1, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 1)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 17, locLine = 2, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 19, locLine = 2, locCol = 2}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 1)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 20, locLine = 2, locCol = 3}
                        , endLoc = Spanned.Loc {locBytesPos = 24, locLine = 2, locCol = 7}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitInteger 103)
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 24, locLine = 2, locCol = 7}
                        , endLoc = Spanned.Loc {locBytesPos = 29, locLine = 2, locCol = 12}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitRational ((-40) % 1))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 29, locLine = 2, locCol = 12}
                        , endLoc = Spanned.Loc {locBytesPos = 31, locLine = 2, locCol = 14}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "a0"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 31, locLine = 2, locCol = 14}
                        , endLoc = Spanned.Loc {locBytesPos = 32, locLine = 2, locCol = 15}
                        }
                    , unSpanned = Token.TokLexeme Token.SpComma
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 32, locLine = 2, locCol = 15}
                        , endLoc = Spanned.Loc {locBytesPos = 37, locLine = 2, locCol = 20}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitRational (400000000000 % 1))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 37, locLine = 2, locCol = 20}
                        , endLoc = Spanned.Loc {locBytesPos = 39, locLine = 2, locCol = 22}
                        }
                    , unSpanned = Token.TokLexeme (Token.IdVarId (TextId.stringLit "o0"))
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 39, locLine = 2, locCol = 22}
                        , endLoc = Spanned.Loc {locBytesPos = 40, locLine = 2, locCol = 23}
                        }
                    , unSpanned = Token.TokLexeme Token.SpBrackClose
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 41, locLine = 3, locCol = 0}
                        , endLoc = Spanned.Loc {locBytesPos = 42, locLine = 3, locCol = 1}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenOpen
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 42, locLine = 3, locCol = 1}
                        , endLoc = Spanned.Loc {locBytesPos = 51, locLine = 3, locCol = 10}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitByteString do byteString "a\NUL")
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 52, locLine = 3, locCol = 11}
                        , endLoc = Spanned.Loc {locBytesPos = 60, locLine = 3, locCol = 19}
                        }
                    , unSpanned = Token.TokLexeme (Token.LitChar '\DC1')
                    }
                , Spanned.Spanned
                    { getSpan = Spanned.Span
                        { beginLoc = Spanned.Loc {locBytesPos = 60, locLine = 3, locCol = 19}
                        , endLoc = Spanned.Loc {locBytesPos = 61, locLine = 3, locCol = 20}
                        }
                    , unSpanned = Token.TokLexeme Token.SpParenClose
                    }
                ]

    describe "source2LexTokens .| Layout.preParseForPart" do
        it "should convert the sample source" do
            let (reports, tokens) = TestRunner.runTestRunner
                    do Conduit.sourceToList
                        do source2LexTokens sampleSource
                            Conduit..| Layout.preParseForPart
            otoList reports `shouldBe` []
            [ TokenWithLForTests.fromTokenWithL t | t <- tokens ] `shouldBe`
                [ TokenWithLForTests.Token Token.SpBrackOpen
                , TokenWithLForTests.Token (Token.IdVarId (TextId.stringLit "id"))
                , TokenWithLForTests.Newline (Layout.PositionByCol 0)
                , TokenWithLForTests.Token (Token.IdVarId (TextId.stringLit "x"))
                , TokenWithLForTests.Token Token.SpParenOpen
                , TokenWithLForTests.Token (Token.LitInteger 0)
                , TokenWithLForTests.Token Token.SpParenClose
                , TokenWithLForTests.Token (Token.IdVarId (TextId.stringLit "aa00"))
                , TokenWithLForTests.Token Token.SpComma
                , TokenWithLForTests.Token (Token.LitInteger 1)
                , TokenWithLForTests.Newline (Layout.PositionByCol 0)
                , TokenWithLForTests.Token (Token.LitInteger 1)
                , TokenWithLForTests.Token (Token.LitInteger 103)
                , TokenWithLForTests.Token (Token.LitRational ((-40) % 1))
                , TokenWithLForTests.Token (Token.IdVarId (TextId.stringLit "a0"))
                , TokenWithLForTests.Token Token.SpComma
                , TokenWithLForTests.Token (Token.LitRational (400000000000 % 1))
                , TokenWithLForTests.Token (Token.IdVarId (TextId.stringLit "o0"))
                , TokenWithLForTests.Token Token.SpBrackClose
                , TokenWithLForTests.Newline (Layout.PositionByCol 0)
                , TokenWithLForTests.Token Token.SpParenOpen
                , TokenWithLForTests.Token (Token.LitByteString do byteString "a\NUL")
                , TokenWithLForTests.Token (Token.LitChar '\DC1')
                , TokenWithLForTests.Token Token.SpParenClose
                ]

