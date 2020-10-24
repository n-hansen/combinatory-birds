module Combinators.ZipperTest exposing (suite)

import Combinators exposing (..)
import Combinators.Zipper exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Test exposing (..)


suite : Test
suite =
    describe "Expr zipper"
        [ test "zipper traversal" <|
            \_ ->
                example1
                    |> exprZipper
                    |> List.iterate next
                    |> List.map read
                    |> Expect.equal
                        ([ "AB(CdE)"
                         , "AB"
                         , "A"
                         , "B"
                         , "CdE"
                         , "Cd"
                         , "C"
                         , "d"
                         , "E"
                         ]
                            |> List.map (parseExpr >> Result.withDefault (Term () ""))
                        )
        , test "editing" <|
            \_ ->
                example1
                    |> exprZipper
                    |> down
                    |> Maybe.andThen right
                    |> Maybe.andThen down
                    |> Maybe.map (edit (always <| Term () "Foo"))
                    |> Maybe.map root
                    |> Expect.equal (parseExpr "AB([Foo]E)" |> Result.toMaybe)
        ]


example1 : PlainExpr
example1 =
    parseExpr "AB(CdE)"
        |> Result.withDefault (Term () "")
