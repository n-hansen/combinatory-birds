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
import Tuple


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
                "failed match 3"
                "Axx"
                "ABC"
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
            , assertMatchExpr
                "repeated variables"
                "Axx(yzy)"
                "ABB((CD)E(CD))"
                (Just
                    [ ( "x", "B" )
                    , ( "y", "CD" )
                    , ( "z", "E" )
                    ]
                )
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
        , describe "rewriting annotations"
            [ assertApplyRulesOnceRewriteData
                "immediate application"
                "Ix=x."
                (Appl emptyRewriteData
                    (Term emptyRewriteData "I")
                    (Term emptyRewriteData "A")
                )
                (Appl { emptyRewriteData | rewrittenTo = Just 0 }
                    (Term emptyRewriteData "I")
                    (Term emptyRewriteData "A")
                )
                (Term { emptyRewriteData | rewrittenFrom = Just 0 } "A")
            , assertApplyRulesOnceRewriteData
                "nested application"
                "Ix=x."
                (Appl emptyRewriteData
                    (Term emptyRewriteData "S")
                    (Appl emptyRewriteData
                        (Term emptyRewriteData "I")
                        (Term emptyRewriteData "A")
                    )
                )
                (Appl emptyRewriteData
                    (Term emptyRewriteData "S")
                    (Appl { emptyRewriteData | rewrittenTo = Just 0 }
                        (Term emptyRewriteData "I")
                        (Term emptyRewriteData "A")
                    )
                )
                (Appl emptyRewriteData
                    (Term emptyRewriteData "S")
                    (Term { emptyRewriteData | rewrittenFrom = Just 0 } "A")
                )
            , assertApplyRulesOnceRewriteData
                "correct rule indexing"
                "Mx=xx.Ix=x."
                (Appl emptyRewriteData
                    (Term emptyRewriteData "I")
                    (Term emptyRewriteData "A")
                )
                (Appl { emptyRewriteData | rewrittenTo = Just 1 }
                    (Term emptyRewriteData "I")
                    (Term emptyRewriteData "A")
                )
                (Term { emptyRewriteData | rewrittenFrom = Just 1 } "A")
            ]
        , describe "rule mangling"
            [ test "example 1" <|
                \_ ->
                    parseRewriteRule "Axy = yx"
                        |> Result.map (mangleUnboundVars "0")
                        |> Expect.all
                            [ Expect.ok
                            , Expect.equal (parseRewriteRule "Axy=yx")
                            ]
            , test "example 2" <|
                \_ ->
                    parseRewriteRule "x = Kxy"
                        |> Result.map (mangleUnboundVars "0")
                        |> Expect.all
                            [ Expect.ok
                            , Expect.equal (parseRewriteRule "x = Kxy0")
                            ]
            , test "example 3" <|
                \_ ->
                    parseRewriteRule "Ax = Cx(By)z"
                        |> Result.map (mangleUnboundVars "1")
                        |> Expect.all
                            [ Expect.ok
                            , Expect.equal (parseRewriteRule "Ax = Cx(By1)z1")
                            ]
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
            Maybe.andThen3 tryRule
                (parseRewriteRule rule |> Result.toMaybe)
                (Just 42)
                (parseExpr input |> Result.toMaybe)
                |> Expect.equal
                    (expect
                        |> Maybe.andThen (parseExpr >> Result.toMaybe)
                        |> Maybe.map
                            (mapExpr (always emptyRewriteData)
                                >> updateExpr
                                    (always
                                        { rewrittenFrom = Just 42
                                        , rewrittenTo = Nothing
                                        }
                                    )
                            )
                    )


assertApplyRulesOnce : String -> String -> String -> Maybe String -> Test
assertApplyRulesOnce name rules input expect =
    test name <|
        \_ ->
            let
                parsedInput =
                    parseExpr input |> Result.toMaybe
            in
            Maybe.andThen2 applyRulesOnce
                (parseRuleset rules
                    |> Result.toMaybe
                    |> Maybe.map RuleList
                )
                (parsedInput
                    |> Maybe.map (mapExpr (always emptyRewriteData))
                )
                |> Maybe.map
                    (Tuple.mapBoth
                        (mapExpr (always ()))
                        (mapExpr (always ()))
                    )
                |> Expect.equal
                    (Maybe.map2 (\x y -> ( x, y ))
                        parsedInput
                        (expect
                            |> Maybe.andThen (parseExpr >> Result.toMaybe)
                        )
                    )


assertApplyRulesOnceRewriteData : String -> String -> RewrittenExpr -> RewrittenExpr -> RewrittenExpr -> Test
assertApplyRulesOnceRewriteData name rules input expectFrom expectTo =
    test name <|
        \_ ->
            let
                parsedRules =
                    parseRuleset rules
                        |> Result.toMaybe
                        |> Maybe.map RuleList
            in
            Maybe.andThen2 applyRulesOnce
                parsedRules
                (Just input)
                |> Expect.equal
                    (Just ( expectFrom, expectTo ))


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
