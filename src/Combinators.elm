module Combinators exposing
    ( Expr(..)
    , ParseError
    , PlainExpr
    , Renderer
    , RewriteData
    , RewriteRule
    , RewrittenExpr
    , Ruleset
    , applyRulesOnce
    , emptyRewriteData
    , makeRuleList
    , makeSingletonRule
    , mangleUnboundVars
    , mapExpr
    , matchExpr
    , parseExpr
    , parseRewriteRule
    , parseRuleset
    , pprintExpr
    , renderExpr
    , reverseRule
    , tryRule
    , updateExpr
    )

import Char
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Result
import Set exposing (Set)
import String



-- Types


type Expr a
    = Term a String
    | FreeVar a String
    | Appl a (Expr a) (Expr a)


type alias PlainExpr =
    Expr ()


type alias RewrittenExpr =
    Expr RewriteData


type alias RewriteRule =
    { pattern : PlainExpr
    , replacement : PlainExpr
    }


reverseRule : RewriteRule -> RewriteRule
reverseRule r =
    { pattern = r.replacement
    , replacement = r.pattern
    }


type alias Ruleset =
    List
        { rule : RewriteRule
        , ix : Int
        }


makeRuleList : List RewriteRule -> Ruleset
makeRuleList =
    List.indexedMap <|
        \ix rule ->
            { ix = ix, rule = rule }


makeSingletonRule : Int -> RewriteRule -> Ruleset
makeSingletonRule ix rule =
    List.singleton
        { ix = ix, rule = rule }


type alias RewriteData =
    { rewrittenFrom : Maybe Int
    , rewrittenTo : Maybe Int
    }


emptyRewriteData : RewriteData
emptyRewriteData =
    { rewrittenFrom = Nothing, rewrittenTo = Nothing }


type alias ParseError =
    ( String, List DeadEnd )


mapExpr : (a -> b) -> Expr a -> Expr b
mapExpr f e =
    case e of
        Term a t ->
            Term (f a) t

        FreeVar a v ->
            FreeVar (f a) v

        Appl a x y ->
            Appl (f a) (mapExpr f x) (mapExpr f y)


updateExpr : (a -> a) -> Expr a -> Expr a
updateExpr f e =
    case e of
        Term a t ->
            Term (f a) t

        FreeVar a v ->
            FreeVar (f a) v

        Appl a x y ->
            Appl (f a) x y



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
    Set.fromList
        [ '\''
        , '*'
        , '~'
        , '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        , '9'
        ]


parseExpr : String -> Result ParseError PlainExpr
parseExpr input =
    Parser.run (expr |. Parser.end) input
        |> Result.mapError (\e -> ( input, e ))


rewriteRule : Parser RewriteRule
rewriteRule =
    Parser.succeed (\x y -> { pattern = x, replacement = y })
        |= expr
        |. Parser.oneOf
            [ Parser.symbol "="
            , Parser.symbol "->"
            ]
        |. whitespace
        |= expr


parseRewriteRule : String -> Result ParseError RewriteRule
parseRewriteRule input =
    Parser.run (rewriteRule |. Parser.end) input
        |> Result.mapError (\e -> ( input, e ))


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


parseRuleset : String -> Result ParseError (List RewriteRule)
parseRuleset input =
    Parser.run (rewriteRuleset |. Parser.end) input
        |> Result.mapError (\e -> ( input, e ))



-- Pretty Printers


type alias Renderer a b =
    { term : a -> String -> b
    , freeVar : a -> String -> b
    , appl : Bool -> a -> b -> b -> b
    }


renderExpr : Renderer a b -> Expr a -> b
renderExpr r e =
    renderExprHelper r e False


renderExprHelper : Renderer a b -> Expr a -> Bool -> b
renderExprHelper r e needsParens =
    case e of
        Term a t ->
            r.term a t

        FreeVar a v ->
            r.freeVar a v

        Appl a x y ->
            r.appl needsParens
                a
                (renderExprHelper r x False)
                (renderExprHelper r y True)


pprintExpr : Expr a -> String
pprintExpr =
    renderExpr
        { term =
            \_ t ->
                if
                    String.toList t
                        |> List.drop 1
                        |> List.all (\c -> Set.member c termSyms)
                then
                    t

                else
                    "[" ++ t ++ "]"
        , freeVar =
            \_ v ->
                if
                    String.toList v
                        |> List.drop 1
                        |> List.all (\c -> Set.member c termSyms)
                then
                    v

                else
                    "[" ++ v ++ "]"
        , appl =
            \needsParens _ x y ->
                if needsParens then
                    "(" ++ x ++ y ++ ")"

                else
                    x ++ y
        }



-- Rewriting


mangleUnboundVars : String -> RewriteRule -> RewriteRule
mangleUnboundVars suffix { pattern, replacement } =
    let
        enumerateVars e seen =
            case e of
                Term _ _ ->
                    seen

                FreeVar _ v ->
                    Set.insert v seen

                Appl _ x y ->
                    seen
                        |> enumerateVars x
                        |> enumerateVars y

        boundVars =
            enumerateVars pattern Set.empty

        updateReplacement e =
            case e of
                Term _ _ ->
                    e

                FreeVar a v ->
                    if Set.member v boundVars then
                        e

                    else
                        FreeVar a (v ++ suffix)

                Appl a x y ->
                    Appl a (updateReplacement x) (updateReplacement y)
    in
    { pattern = pattern
    , replacement = updateReplacement replacement
    }


matchExpr : PlainExpr -> Expr a -> Maybe (Dict String PlainExpr)
matchExpr =
    matchExprHelper Dict.empty


matchExprHelper : Dict String PlainExpr -> PlainExpr -> Expr a -> Maybe (Dict String PlainExpr)
matchExprHelper bindings pattern toMatch =
    case ( pattern, toMatch ) of
        ( FreeVar () v, val ) ->
            case Dict.get v bindings of
                Just bound ->
                    if bound == mapExpr (always ()) val then
                        Just bindings

                    else
                        Nothing

                Nothing ->
                    bindings
                        |> Dict.insert v (mapExpr (always ()) val)
                        |> Just

        ( Term () t1, Term _ t2 ) ->
            if t1 == t2 then
                Just bindings

            else
                Nothing

        ( Appl () x1 y1, Appl _ x2 y2 ) ->
            matchExprHelper bindings x1 x2
                |> Maybe.andThen
                    (\newBindings -> matchExprHelper newBindings y1 y2)

        ( _, _ ) ->
            Nothing


tryRule : RewriteRule -> Int -> Expr b -> Maybe RewrittenExpr
tryRule { pattern, replacement } tag e =
    matchExpr pattern e
        |> Maybe.map
            (performSubstitutions replacement
                >> mapExpr (always emptyRewriteData)
                >> updateExpr (always { rewrittenFrom = Just tag, rewrittenTo = Nothing })
            )


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


applyRulesOnce : Ruleset -> RewrittenExpr -> Maybe ( RewrittenExpr, RewrittenExpr )
applyRulesOnce rules toRewrite =
    let
        runRules =
            rules
                |> List.map
                    (\{ ix, rule } ->
                        \_ ->
                            tryRule rule ix toRewrite
                                |> Maybe.map (\rw -> ( rw, ix ))
                    )
    in
    case Maybe.orListLazy runRules of
        Just ( rewritten, tag ) ->
            Just
                ( toRewrite
                    |> updateExpr (\rw -> { rw | rewrittenTo = Just tag })
                , rewritten
                )

        Nothing ->
            case toRewrite of
                Term _ _ ->
                    Nothing

                FreeVar _ _ ->
                    Nothing

                Appl a x y ->
                    Maybe.orListLazy
                        [ \_ ->
                            applyRulesOnce rules x
                                |> Maybe.map
                                    (\( from, to ) ->
                                        ( Appl a from y
                                        , Appl emptyRewriteData to (mapExpr (always emptyRewriteData) y)
                                        )
                                    )
                        , \_ ->
                            applyRulesOnce rules y
                                |> Maybe.map
                                    (\( from, to ) ->
                                        ( Appl a x from
                                        , Appl emptyRewriteData (mapExpr (always emptyRewriteData) x) to
                                        )
                                    )
                        ]
