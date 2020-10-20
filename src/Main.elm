module Main exposing (..)

import Char
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Result
import Set


type Expr a
    = Term a String
    | Appl a (Expr a) (Expr a)


mapExpr : (a -> b) -> Expr a -> Expr b
mapExpr f e =
    case e of
        Term a t -> Term (f a) t
        Appl a x y -> Appl (f a) (mapExpr f x) (mapExpr f y)


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


type TermType
    = Free
    | Bound


type alias MatchExpr =
    Expr TermType


matchExpr : MatchExpr -> PlainExpr -> Maybe (Dict String PlainExpr)
matchExpr pattern toMatch =
    case ( pattern, toMatch ) of
        ( Term Free t, val ) ->
            Just <| Dict.singleton t val

        ( Term Bound t1, Term () t2 ) ->
            if t1 == t2 then
                Just Dict.empty

            else
                Nothing

        ( Appl _ x1 y1, Appl () x2 y2 ) ->
            matchExpr x1 x2
                |> Maybe.andThen
                    (\x ->
                        matchExpr y1 y2
                            |> Maybe.map (Dict.union x)
                    )

        ( _, _ ) ->
            Nothing


freeAllTermsButHead : PlainExpr -> MatchExpr
freeAllTermsButHead e =
    case e of
        Term () t -> Term Bound t
        Appl () x y -> Appl Bound (freeAllTermsButHead x) (mapExpr (\_ -> Free) y)



