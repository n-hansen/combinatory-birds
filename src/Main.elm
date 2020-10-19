module Main exposing (..)

import Char
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Result
import Set


type Expr a
    = Term a String
    | Appl a (Expr a) (Expr a)


type alias PlainExpr =
    Expr ()


expr : Parser PlainExpr
expr =
    singleExpr
        |> Parser.andThen
            (\e -> Parser.loop e exprHelper)


exprHelper : Expr () -> Parser (Parser.Step PlainExpr PlainExpr)
exprHelper soFar =
    Parser.oneOf
        [ Parser.succeed (\next -> Parser.Loop <| Appl () soFar next)
            |= singleExpr
        , Parser.succeed (Parser.Done soFar)
        ]


singleExpr : Parser PlainExpr
singleExpr =
    Parser.oneOf
        [ Parser.succeed (Term ())
            |= Parser.variable
                { start = Char.isAlpha
                , inner = always False
                , reserved = Set.empty
                }
        , Parser.succeed (Term ())
            |. Parser.symbol "["
            |= Parser.variable
                { start = \c -> c /= ']'
                , inner = \c -> c /= ']'
                , reserved = Set.empty
                }
            |. Parser.symbol "]"
        , Parser.succeed identity
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> expr)
            |. Parser.symbol ")"
        ]
        |. Parser.spaces


parseExpr : String -> Result String PlainExpr
parseExpr input =
    Parser.run expr input
        |> Result.mapError Parser.deadEndsToString
