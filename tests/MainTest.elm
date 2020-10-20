module MainTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List
import Main exposing (..)
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
