module Combinators exposing
  ( Expr(..)
  , PlainExpr
  , RewriteRule
  , parseExpr
  , parseRewriteRule
  , matchExpr
  , tryRule
  , applyRulesOnce
  )

import Char
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Result
import Set exposing (Set)


type Expr a
    = Term a String
    | FreeVar a String
    | Appl a (Expr a) (Expr a)


mapExpr : (a -> b) -> Expr a -> Expr b
mapExpr f e =
    case e of
        Term a t -> Term (f a) t
        FreeVar a v -> FreeVar (f a) v
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
                { start = Char.isUpper
                , inner = \c -> Set.member c termSyms
                , reserved = Set.empty
                }
        , Parser.succeed (FreeVar ())
            |= Parser.variable
                { start = Char.isLower
                , inner = \c -> Set.member c termSyms
                , reserved = Set.empty
                }
        , Parser.succeed identity
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> expr)
            |. Parser.symbol ")"
        ]
        |. Parser.spaces


termSyms : Set Char
termSyms = Set.fromList ['\'', '*', '`', '~']


parseExpr : String -> Result String PlainExpr
parseExpr input =
    Parser.run expr input
        |> Result.mapError Parser.deadEndsToString


matchExpr : PlainExpr -> PlainExpr -> Maybe (Dict String PlainExpr)
matchExpr pattern toMatch =
    case ( pattern, toMatch ) of
        ( FreeVar () v, val ) ->
            Just <| Dict.singleton v val

        ( Term () t1, Term () t2 ) ->
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


type alias RewriteRule =
    { pattern : PlainExpr
    , replacement : PlainExpr
    }


rewriteRule : Parser RewriteRule
rewriteRule =
    Parser.succeed (\x y -> { pattern = x, replacement = y })
        |= expr
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= expr
        |. Parser.spaces
        |. Parser.oneOf [Parser.symbol ".", Parser.symbol ";"]


parseRewriteRule : String -> Result String RewriteRule
parseRewriteRule input =
    Parser.run rewriteRule input
        |> Result.mapError Parser.deadEndsToString


tryRule : RewriteRule -> PlainExpr -> Maybe PlainExpr
tryRule { pattern, replacement } e =
    matchExpr pattern e
        |> Maybe.map (performSubstitutions replacement)


performSubstitutions : PlainExpr -> Dict String PlainExpr -> PlainExpr
performSubstitutions e bindings =
    case e of
        Term () t ->
            Term () t

        FreeVar () v ->
            Dict.get v bindings
                |> Maybe.withDefault e

        Appl () x y ->
            Appl () (performSubstitutions x bindings) (performSubstitutions y bindings)


applyRulesOnce : List RewriteRule -> PlainExpr -> Maybe PlainExpr
applyRulesOnce rules toRewrite =
    case
        rules
            |> List.map ( \rule -> \_ -> tryRule rule toRewrite )
            |> Maybe.orListLazy
    of
        Just rewritten -> Just rewritten
        Nothing ->
            case toRewrite of
                Term () _ -> Nothing

                FreeVar () _ -> Nothing

                Appl () x y ->
                    Maybe.orListLazy
                        [ \_ ->
                              applyRulesOnce rules x
                              |> Maybe.map (\rw -> Appl () rw y)
                        , \_ ->
                              applyRulesOnce rules y
                              |> Maybe.map (\rw -> Appl () x rw)
                        ]
