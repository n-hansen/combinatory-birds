module CombinatorsTest exposing (suite)

import Combinators exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Test exposing (..)


suite : Test
suite =
    describe "main test suite"
        [ exprSuite
        ]


exprSuite : Test
exprSuite =
    describe "Expr handling"
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
                "Ix=x."
                "IA"
                (Just "A")
            , assertTryRule
                "K"
                "Kxy=x."
                "KBA"
                (Just "B")
            , assertTryRule
                "S"
                "Sxyz=xz(yz)."
                "SKSK"
                (Just "KK(SK)")
            , assertTryRule
                "failed match"
                "Ix=x."
                "III"
                Nothing
            ]
        , describe "recursive rule application"
            [ assertApplyRulesOnce
                  "immediate application"
                  ["Ix=x."]
                  "IA"
                  (Just "A")
            , assertApplyRulesOnce
                  "multiple rules"
                  ["Ix=x.", "Kxy=x."]
                  "KAB"
                  (Just "A")
            , assertApplyRulesOnce
                  "recursive Application"
                  ["Ix=x."]
                  "A(IXY)"
                  (Just "A(XY)")
            , assertApplyRulesOnce
                  "multiple rules 2"
                  ["Ix=x.", "Kxy=x."]
                  "A(KII)"
                  (Just "AI")
            , assertApplyRulesOnce
                  "failed match"
                  ["Ix=x."]
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
                (parseExpr pattern
                    |> Result.map freeAllTermsButHead
                    |> Result.toMaybe
                )
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


assertApplyRulesOnce : String -> List String -> String -> Maybe String -> Test
assertApplyRulesOnce name rules input expect =
    test name <|
        \_ ->
            Maybe.andThen2 applyRulesOnce
                ( rules
                      |> List.map (parseRewriteRule >> Result.toMaybe)
                      |> Maybe.combine
                )
                ( parseExpr input |> Result.toMaybe )
                |> Expect.equal (expect |> Maybe.andThen (parseExpr >> Result.toMaybe))
