module CombinatorsTest exposing (suite)

import Char
import Combinators exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List
import Maybe
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Result
import Result.Extra as Result
import Shrink
import String
import Test exposing (..)


suite : Test
suite =
    describe "Combinators"
        [ describe "parser"
            [ assertParseResult
                "single term"
                "A"
                (Term () "A")
            , assertParseResult
                "single application"
                "AB"
                (Appl () (Term () "A") (Term () "B"))
            , assertParseResult
                "multiple implicit application"
                "ABC"
                (Appl ()
                    (Appl ()
                        (Term () "A")
                        (Term () "B")
                    )
                    (Term () "C")
                )
            , assertParseResult
                "multiple explicit application"
                "A(BC)"
                (Appl ()
                    (Term () "A")
                    (Appl ()
                        (Term () "B")
                        (Term () "C")
                    )
                )
            , assertParseResult
                "free variables"
                "Ax"
                (Appl ()
                    (Term () "A")
                    (FreeVar () "x")
                )
            , assertParseResult
                "long names"
                "A'b*'c~~"
                (Appl ()
                    (Appl ()
                        (Term () "A'")
                        (FreeVar () "b*'")
                    )
                    (FreeVar () "c~~")
                )
            , assertParseResult
                "bracketed identifiers"
                "[Abcd] [efgH]"
                (Appl ()
                    (Term () "Abcd")
                    (FreeVar () "efgH")
                )
            , assertParseResult
                "whitespace"
                "A\nB#foo\n(C--bar\nD\n   )"
                (Appl ()
                    (Appl ()
                        (Term () "A")
                        (Term () "B")
                    )
                    (Appl ()
                        (Term () "C")
                        (Term () "D")
                    )
                )
            ]
        , describe "printer"
            [ fuzz exprFuzzer "pretty printer roundtrips successfully" <|
                \expr ->
                    expr
                        |> pprintExpr
                        |> parseExpr
                        |> Expect.equal (Ok expr)
            , test "non-essential parens are elided" <|
                \_ ->
                    Appl ()
                        (Appl ()
                            (Term () "A")
                            (Term () "B")
                        )
                        (Term () "C")
                        |> pprintExpr
                        |> Expect.equal "ABC"
            ]
        , describe "matching"
            [ assertMatchExpr
                "singleton"
                "A"
                "A"
                (Just [])
            , assertMatchExpr
                "failed match"
                "A"
                "B"
                Nothing
            , assertMatchExpr
                "failed match 2"
                "A"
                "AB"
                Nothing
            , assertMatchExpr
                "single bound variable"
                "Ax"
                "AB"
                (Just [ ( "x", "B" ) ])
            , assertMatchExpr
                "multiple bound variables"
                "A(xy)"
                "A(BC)"
                (Just [ ( "x", "B" ), ( "y", "C" ) ])
            , assertMatchExpr
                "bound subtree"
                "Axy"
                "A(BCD)(E(FG))"
                (Just [ ( "x", "BCD" ), ( "y", "E(FG)" ) ])
            ]
        , describe "direct rule application"
            [ assertTryRule
                "I"
                "Ix=x"
                "IA"
                (Just "A")
            , assertTryRule
                "K"
                "Kxy=x"
                "KBA"
                (Just "B")
            , assertTryRule
                "S"
                "Sxyz=xz(yz)"
                "SKSK"
                (Just "KK(SK)")
            , assertTryRule
                "failed match"
                "Ix=x"
                "III"
                Nothing
            ]
        , describe "recursive rule application"
            [ assertApplyRulesOnce
                "immediate application"
                "Ix=x."
                "IA"
                (Just "A")
            , assertApplyRulesOnce
                "multiple rules"
                "Ix=x. Kxy=x."
                "KAB"
                (Just "A")
            , assertApplyRulesOnce
                "recursive Application"
                "Ix=x."
                "A(IXY)"
                (Just "A(XY)")
            , assertApplyRulesOnce
                "multiple rules 2"
                "Ix=x. Kxy=x."
                "A(KII)"
                (Just "AI")
            , assertApplyRulesOnce
                "failed match"
                "Ix=x."
                "XYZ(A(BI))"
                Nothing
            ]
        ]


assertParseResult : String -> String -> PlainExpr -> Test
assertParseResult name input expect =
    test name <|
        \_ ->
            parseExpr input
                |> Expect.equal (Ok expect)


assertMatchExpr : String -> String -> String -> Maybe (List ( String, String )) -> Test
assertMatchExpr name pattern toMatch expect =
    test name <|
        \_ ->
            Maybe.andThen2
                matchExpr
                (parseExpr pattern |> Result.toMaybe)
                (parseExpr toMatch |> Result.toMaybe)
                |> Expect.equal
                    (expect
                        |> Maybe.andThen
                            (List.map
                                (\( t, e ) ->
                                    parseExpr e
                                        |> Result.toMaybe
                                        |> Maybe.map (\pe -> ( t, pe ))
                                )
                                >> Maybe.combine
                                >> Maybe.map Dict.fromList
                            )
                    )


assertTryRule : String -> String -> String -> Maybe String -> Test
assertTryRule name rule input expect =
    test name <|
        \_ ->
            Maybe.andThen2 tryRule
                (parseRewriteRule rule |> Result.toMaybe)
                (parseExpr input |> Result.toMaybe)
                |> Expect.equal (expect |> Maybe.andThen (parseExpr >> Result.toMaybe))


assertApplyRulesOnce : String -> String -> String -> Maybe String -> Test
assertApplyRulesOnce name rules input expect =
    test name <|
        \_ ->
            Maybe.andThen2 applyRulesOnce
                (parseRuleset rules
                    |> Result.toMaybe
                )
                (parseExpr input |> Result.toMaybe)
                |> Expect.equal (expect |> Maybe.andThen (parseExpr >> Result.toMaybe))


randomExpr : Generator PlainExpr
randomExpr =
    Random.uniform
        (Random.int 65 90
            |> Random.map Char.fromCode
            |> Random.andThen
                (\firstChar ->
                    Random.uniform
                        (String.fromChar firstChar)
                        [ String.fromList [ firstChar, '~' ]
                        , String.fromList [ firstChar, '*', '*' ]
                        , String.fromList [ firstChar, '0' ]
                        , String.fromList [ firstChar, 'x' ]
                        , String.fromList [ firstChar, 'Y' ]
                        ]
                )
            |> Random.map (Term ())
        )
        [ Random.int 97 122
            |> Random.map Char.fromCode
            |> Random.andThen
                (\firstChar ->
                    Random.uniform
                        (String.fromChar firstChar)
                        [ String.fromList [ firstChar, '`' ]
                        , String.fromList [ firstChar, '\'', '\'' ]
                        , String.fromList [ firstChar, '0' ]
                        , String.fromList [ firstChar, 'a' ]
                        , String.fromList [ firstChar, 'B' ]
                        ]
                )
            |> Random.map (FreeVar ())
        , Random.map2 (Appl ())
            (Random.lazy (\_ -> randomExpr))
            (Random.lazy (\_ -> randomExpr))
        ]
        |> Random.andThen identity


exprFuzzer : Fuzzer PlainExpr
exprFuzzer =
    Fuzz.custom randomExpr Shrink.noShrink
