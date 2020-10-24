module Combinators exposing
    ( Expr(..)
    , PlainExpr
    , RewriteRule
    , applyRulesOnce
    , matchExpr
    , parseExpr
    , parseRewriteRule
    , parseRuleset
    , tryRule
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



-- Types


type Expr a
    = Term a String
    | FreeVar a String
    | Appl a (Expr a) (Expr a)


type alias PlainExpr =
    Expr ()


type alias RewriteRule =
    { pattern : PlainExpr
    , replacement : PlainExpr
    }


mapExpr : (a -> b) -> Expr a -> Expr b
mapExpr f e =
    case e of
        Term a t ->
            Term (f a) t

        FreeVar a v ->
            FreeVar (f a) v

        Appl a x y ->
            Appl (f a) (mapExpr f x) (mapExpr f y)



-- Parsers


whitespace : Parser ()
whitespace =
    let
        ifProgress parser offset =
            Parser.succeed identity
                |. parser
                |= Parser.getOffset
                |> Parser.map
                    (\newOffset ->
                        if offset == newOffset then
                            Parser.Done ()

                        else
                            Parser.Loop newOffset
                    )
    in
    Parser.loop 0 <|
        ifProgress <|
            Parser.oneOf
                [ Parser.lineComment "//"
                , Parser.lineComment "#"
                , Parser.lineComment "--"
                , Parser.spaces
                ]


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
            |. whitespace
            |= Parser.lazy (\_ -> expr)
            |. Parser.symbol ")"
        , Parser.succeed identity
            |. Parser.symbol "["
            |= Parser.oneOf
                [ Parser.variable
                    { start = Char.isLower
                    , inner = \c -> c /= ']'
                    , reserved = Set.empty
                    }
                    |> Parser.map (FreeVar ())
                , Parser.variable
                    { start = \c -> not (Char.isLower c) && c /= ']'
                    , inner = \c -> c /= ']'
                    , reserved = Set.empty
                    }
                    |> Parser.map (Term ())
                ]
            |. Parser.symbol "]"
        ]
        |. whitespace


termSyms : Set Char
termSyms =
    Set.fromList [ '\'', '*', '`', '~' ]


parseExpr : String -> Result String PlainExpr
parseExpr input =
    Parser.run (expr |. Parser.end) input
        |> Result.mapError Parser.deadEndsToString


rewriteRule : Parser RewriteRule
rewriteRule =
    Parser.succeed (\x y -> { pattern = x, replacement = y })
        |= expr
        |. Parser.symbol "="
        |. whitespace
        |= expr


parseRewriteRule : String -> Result String RewriteRule
parseRewriteRule input =
    Parser.run (rewriteRule |. Parser.end) input
        |> Result.mapError Parser.deadEndsToString


rewriteRuleset : Parser (List RewriteRule)
rewriteRuleset =
    Parser.sequence
        { start = ""
        , separator = "."
        , end = ""
        , spaces = whitespace
        , item = rewriteRule
        , trailing = Parser.Mandatory
        }


parseRuleset : String -> Result String (List RewriteRule)
parseRuleset input =
    Parser.run (rewriteRuleset |. Parser.end) input
        |> Result.mapError Parser.deadEndsToString



-- Rewriting


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
            |> List.map (\rule -> \_ -> tryRule rule toRewrite)
            |> Maybe.orListLazy
    of
        Just rewritten ->
            Just rewritten

        Nothing ->
            case toRewrite of
                Term () _ ->
                    Nothing

                FreeVar () _ ->
                    Nothing

                Appl () x y ->
                    Maybe.orListLazy
                        [ \_ ->
                            applyRulesOnce rules x
                                |> Maybe.map (\rw -> Appl () rw y)
                        , \_ ->
                            applyRulesOnce rules y
                                |> Maybe.map (\rw -> Appl () x rw)
                        ]
