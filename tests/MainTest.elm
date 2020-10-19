module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)

suite : Test
suite =
    describe "main test suite"
        [ exprSuite ]

exprSuite : Test

exprSuite =
    describe "Expr handling"
        [ describe "parser"
              [ verifyParse
                   "single term"
                    "A"
                    (Term () "A")
              , verifyParse
                    "single application"
                    "AB"
                    (Appl () (Term () "A") (Term () "B"))
              , verifyParse
                    "multiple implicit application"
                    "ABC"
                    (Appl () (Appl () (Term () "A")
                                      (Term () "B"))
                             (Term () "C"))
              , verifyParse
                    "multiple explicit application"
                    "A(BC)"
                    (Appl () (Term () "A")
                             (Appl () (Term () "B")
                                      (Term () "C")))
              ]
        ]

verifyParse : String -> String -> PlainExpr -> Test
verifyParse name input expect =
    test name <| \_ ->
       parseExpr input
           |> Expect.equal (Ok expect)
