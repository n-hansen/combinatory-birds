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
            10 10
            skiRules
            "A"
            "KAB"
            (Just [ "A", "KAB" ])
        , assertSearchForMatch "Apply a nested rule"
            10 10
            skiRules
            "ABC"
            "A(KBC)C"
            (Just [ "ABC", "A(KBC)C" ])
        , assertSearchForMatch "Dead end"
            10 10
            skiRules
            "A"
            "KA"
            Nothing
        , assertSearchForMatch "Free vars"
            10 10
            skiRules
            "xyy"
            "A(KBA)B"
            (Just [ "ABB", "A(KBA)B" ])
        , assertSearchForMatch "Free var failure"
            10 10
            skiRules
            "xyy"
            "A(KAB)B"
            Nothing
        , assertSearchForMatch "Multiple rules"
            10 10
            skiRules
            "A"
            "KISA"
            (Just [ "A", "IA", "KISA" ])
        , assertSearchForMatch "Backtracking"
            10 10
            skiRules
            "KBA"
            "K(KBB)A"
            (Just [ "KBA", "K(KBB)A" ])
        , assertSearchForMatch "Backtracking 2"
            10 10
            skiRules
            "KBB"
            "K(KBB)A"
            (Just [ "KBB", "K(KBB)A" ])
        , assertSearchForMatch "Loop detection"
            10 10
            "A=B.B=A."
            "[Impossible]"
            "A"
            Nothing
        , assertSearchForMatch "Idempotency"
            10 10
            "Kxy=x.x=Kxy."
            "ABC"
            "ABC"
            (Just ["ABC"])
        , assertSearchForMatch "Bails out on depth"
            2 10
            "A=AB."
            "ABBBB"
            "A"
            Nothing
        , assertSearchForMatch "bails out on expr length"
            10 3
            "A=AB."
            "ABBBB"
            "A"
            Nothing
        , test "Rewrite history matches step runner" <| \_ ->
            let
                rules =
                    parseRuleset "Axy=xyy.Ix=x."
                        |> Result.map makeRuleList
                        |> Result.toMaybe

                input =
                    parseExpr "AIB"
                        |> Result.toMaybe

                pattern =
                    parseExpr "BB"
                        |> Result.toMaybe

                searchResult =
                    Maybe.andThen3 (searchForMatch 10 10)
                        rules
                        pattern
                        (input |> Maybe.map (mapExpr <| always emptyRewriteData))

                stepRules = Maybe.andThen
                            (\(head, tail) ->
                                 Maybe.andThen2 applyRulesOnce
                                 rules
                                 (Just head)
                                 |> Maybe.map
                                    (\(from, to) ->
                                         (to, from :: tail)
                                    )
                            )

                stepResult =
                    input
                        |> Maybe.map (\i -> (mapExpr (always emptyRewriteData) i, []))
                        |> stepRules
                        |> stepRules
                        |> Maybe.map (\ (head, tail) -> head :: tail)
            in
                searchResult
                    |> Expect.equal stepResult

        ]


assertSearchForMatch :
    String
    -> Int
    -> Int
    -> String
    -> String
    -> String
    -> Maybe (List String)
    -> Test
assertSearchForMatch name maxDepth maxExprLen rulesS patternS inputS expectS =
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
                Maybe.andThen3 (searchForMatch maxDepth maxExprLen)
                    rules
                    pattern
                    (input |> Maybe.map (mapExpr <| always emptyRewriteData))
                    |> Maybe.map (List.map (mapExpr (always ())))
                    |> Expect.equal expect
        ]


skiRules : String
skiRules =
    "Kxy=x.Sxyz=xz(yz).Ix=x."
