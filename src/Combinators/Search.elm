module Combinators.Search exposing (searchForMatch)

import Combinators exposing (..)
import Combinators.Zipper exposing (..)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Set exposing (Set)



-- Types


type alias SearchContext =
    { rules : Ruleset
    , loc : ExprZip RewriteData
    , path : List RewrittenExpr
    , queue : List ( RewrittenExpr, List RewrittenExpr )
    , nextQueue : List ( RewrittenExpr, List RewrittenExpr )
    , seen : Set String
    , nodesProcessed : Int
    , exprProcessed : Int
    , depth : Int
    }


type SearchState
    = Failed
    | Searching SearchContext
    | FoundIt (List RewrittenExpr)


{-| Nicer alternative to `Result a (Maybe b)`, since using Err to return a success feels dumb.
-}
type MaybeEither a b
    = NothingE
    | Left a
    | Right b


mapME : (b -> c) -> MaybeEither a b -> MaybeEither a c
mapME f me =
    case me of
        NothingE ->
            NothingE

        Left l ->
            Left l

        Right b ->
            Right (f b)


{-| Map over a list, aggregating any `Right`s, dropping any `NothingE`s,
and short-circuiting if we encounter a `Left`.
-}
traverseME : (x -> MaybeEither a b) -> List x -> MaybeEither a (List b)
traverseME f =
    let
        go rights xs =
            case xs of
                [] ->
                    Right <| List.reverse rights

                x :: rest ->
                    case f x of
                        NothingE ->
                            go rights rest

                        Left l ->
                            Left l

                        Right r ->
                            go (r :: rights) rest
    in
    go []


unpackME : (() -> x) -> (a -> x) -> (b -> x) -> MaybeEither a b -> x
unpackME f g h me =
    case me of
        NothingE ->
            f ()

        Left l ->
            g l

        Right r ->
            h r


stepSearch : (RewrittenExpr -> Bool) -> SearchContext -> SearchState
stepSearch test ctx =
    ctx.rules
        |> traverseME
            (\{ rule, ix } ->
                case tryRuleAnnotated rule ix (read ctx.loc) of
                    Nothing ->
                        NothingE

                    Just rewritten ->
                        let
                            newExpr =
                                ctx.loc
                                    |> mapExprZipper (always emptyRewriteData)
                                    |> set rewritten
                                    |> root

                            oldExpr =
                                ctx.loc
                                    |> edit
                                        (updateExpr <|
                                            \rw ->
                                                { rw | rewrittenTo = Just ix }
                                        )
                                    |> root
                        in
                        if test newExpr then
                            Left <|
                                newExpr
                                    :: oldExpr
                                    :: ctx.path

                        else
                            Right ( newExpr, oldExpr )
            )
        |> mapME
            (List.foldl
                (\( rewritten, old ) acc ->
                    let
                        printed =
                            pprintExpr rewritten
                    in
                    if Set.member printed acc.seen then
                        acc

                    else
                        { acc
                            | nextQueue = ( rewritten, old :: ctx.path ) :: acc.nextQueue
                            , seen = Set.insert printed acc.seen
                        }
                )
                ctx
            )
        |> unpackME
            (\_ -> stepSearchAdvanceCursor ctx)
            FoundIt
            stepSearchAdvanceCursor


stepSearchAdvanceCursor : SearchContext -> SearchState
stepSearchAdvanceCursor ctx =
    -- first advance the location
    next ctx.loc
        |> Maybe.map
            (\loc ->
                { ctx
                    | loc = loc
                    , nodesProcessed = ctx.nodesProcessed + 1
                }
            )
        -- otherwise pop a new expr off the queue
        |> Maybe.orElseLazy
            (\_ ->
                List.uncons ctx.queue
                    |> Maybe.map
                        (\( ( expr, path ), rest ) ->
                            { ctx
                                | loc = exprZipper expr
                                , path = path
                                , queue = rest
                                , nodesProcessed = ctx.nodesProcessed + 1
                                , exprProcessed = ctx.exprProcessed + 1
                            }
                        )
            )
        -- otherwise load the next queue
        |> Maybe.unpack
            (\_ ->
                case ctx.nextQueue of
                    [] ->
                        Failed

                    ( expr, path ) :: rest ->
                        Searching
                            { ctx
                                | loc = exprZipper expr
                                , path = path
                                , nextQueue = []
                                , queue = rest
                                , nodesProcessed = ctx.nodesProcessed + 1
                                , exprProcessed = ctx.exprProcessed + 1
                                , depth = ctx.depth + 1
                                , rules =
                                    List.map
                                        (\r ->
                                            { r
                                                | rule =
                                                    mangleUnboundVars "'"
                                                        r.rule
                                            }
                                        )
                                        ctx.rules
                            }
            )
            Searching


searchForMatch : Ruleset -> PlainExpr -> RewrittenExpr -> Maybe (List RewrittenExpr)
searchForMatch rules pattern startingPoint =
    matchExpr pattern startingPoint
        |> Maybe.map
            (\_ ->
                [ startingPoint ]
            )
        |> Maybe.orElseLazy
            (\_ ->
                searchForMatchHelper
                    (matchExpr pattern >> Maybe.isJust)
                    { rules = rules
                    , loc = exprZipper startingPoint
                    , path = []
                    , queue = []
                    , nextQueue = []
                    , seen = Set.empty
                    , nodesProcessed = 0
                    , exprProcessed = 0
                    , depth = 0
                    }
            )


searchForMatchHelper : (RewrittenExpr -> Bool) -> SearchContext -> Maybe (List RewrittenExpr)
searchForMatchHelper test ctx =
    case stepSearch test ctx of
        Failed ->
            Nothing

        Searching newCtx ->
            searchForMatchHelper test newCtx

        FoundIt result ->
            Just result
