module Combinators.SearchTest exposing (suite)

import Combinators exposing (..)
import Combinators.Search exposing (..)
import Expect exposing (Expectation)
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Test exposing (..)


suite : Test
suite =
    describe "Expr rewrite searching"
        [ assertSearchForMatch "Apply a top level rule"
            skiRules
            "A"
            "KAB"
            (Just [ "A", "KAB" ])
        , assertSearchForMatch "Apply a nested rule"
            skiRules
            "ABC"
            "A(KBC)C"
            (Just [ "ABC", "A(KBC)C" ])
        , assertSearchForMatch "Dead end"
            skiRules
            "A"
            "KA"
            Nothing
        , assertSearchForMatch "Free vars"
            skiRules
            "xyy"
            "A(KBA)B"
            (Just [ "ABB", "A(KBA)B" ])
        , assertSearchForMatch "Free var failure"
            skiRules
            "xyy"
            "A(KAB)B"
            Nothing
        , assertSearchForMatch "Multiple rules"
            skiRules
            "A"
            "KISA"
            (Just [ "A", "IA", "KISA" ])
        , assertSearchForMatch "Backtracking"
            skiRules
            "KBA"
            "K(KBB)A"
            (Just [ "KBA", "K(KBB)A" ])
        , assertSearchForMatch "Backtracking 2"
            skiRules
            "KBB"
            "K(KBB)A"
            (Just [ "KBB", "K(KBB)A" ])
        , assertSearchForMatch "Loop detection"
            "A=B.B=A."
            "[Impossible]"
            "A"
            Nothing
        ]


assertSearchForMatch :
    String
    -> String
    -> String
    -> String
    -> Maybe (List String)
    -> Test
assertSearchForMatch name rulesS patternS inputS expectS =
    describe name <|
        let
            rules =
                parseRuleset rulesS
                    |> Result.map makeRuleList
                    |> Result.toMaybe

            input =
                parseExpr inputS
                    |> Result.toMaybe

            pattern =
                parseExpr patternS
                    |> Result.toMaybe

            expect =
                expectS
                    |> Maybe.andThen
                        (List.map (parseExpr >> Result.toMaybe)
                            >> Maybe.combine
                        )
        in
        [ test "sanity check inputs" <|
            \_ ->
                Maybe.combine
                    [ rules
                        |> Maybe.map (always ())
                    , input
                        |> Maybe.map (always ())
                    , pattern
                        |> Maybe.map (always ())
                    ]
                    |> Expect.notEqual Nothing
        , test "assert result" <|
            \_ ->
                Maybe.andThen3 searchForMatch
                    rules
                    pattern
                    input
                    |> Maybe.map (List.map (mapExpr (always ())))
                    |> Expect.equal expect
        ]


skiRules : String
skiRules =
    "Kxy=x.Sxyz=xz(yz).Ix=x."
