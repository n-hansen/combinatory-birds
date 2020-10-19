module Main exposing (..)

import Char
import Parser exposing ((|.), (|=), Parser)
import Result
import Set


type Expr
    = Term String
    | Appl Expr Expr


expr : Parser Expr
expr =
    singleExpr
        |> Parser.andThen
            (\e -> Parser.loop e exprHelper)


exprHelper : Expr -> Parser (Parser.Step Expr Expr)
exprHelper soFar =
    Parser.oneOf
        [ Parser.succeed (\next -> Parser.Loop <| Appl soFar next)
            |= singleExpr
        , Parser.succeed (Parser.Done soFar)
        ]


singleExpr : Parser Expr
singleExpr =
    Parser.oneOf
        [ Parser.succeed Term
            |= Parser.variable
                { start = Char.isAlpha
                , inner = always False
                , reserved = Set.empty
                }
        , Parser.succeed Term
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
