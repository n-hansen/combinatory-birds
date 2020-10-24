module Combinators.Zipper exposing
    ( ExprZip
    , down
    , edit
    , exprZipper
    , left
    , next
    , read
    , right
    , root
    , up
    )

import Combinators exposing (..)
import Maybe.Extra as Maybe


type alias ExprZip a =
    { focus : Expr a
    , context : ExprZ a
    }


type ExprZ a
    = ApplZL a (Expr a) (ExprZ a)
    | ApplZR a (Expr a) (ExprZ a)
    | Root


exprZipper : Expr a -> ExprZip a
exprZipper e =
    { focus = e
    , context = Root
    }


read : ExprZip a -> Expr a
read loc =
    loc.focus


edit : (Expr a -> Expr a) -> ExprZip a -> ExprZip a
edit f loc =
    { loc
        | focus = f loc.focus
    }


up : ExprZip a -> Maybe (ExprZip a)
up loc =
    case loc.context of
        Root ->
            Nothing

        ApplZL a r above ->
            Just
                { focus = Appl a loc.focus r
                , context = above
                }

        ApplZR a l above ->
            Just
                { context = above
                , focus = Appl a l loc.focus
                }


down : ExprZip a -> Maybe (ExprZip a)
down loc =
    case loc.focus of
        Appl a l r ->
            Just
                { context = ApplZL a r loc.context
                , focus = l
                }

        _ ->
            Nothing


left : ExprZip a -> Maybe (ExprZip a)
left loc =
    case loc.context of
        ApplZR a l above ->
            Just
                { context = ApplZL a loc.focus above
                , focus = l
                }

        _ ->
            Nothing


right : ExprZip a -> Maybe (ExprZip a)
right loc =
    case loc.context of
        ApplZL a r above ->
            Just
                { context = ApplZR a loc.focus above
                , focus = r
                }

        _ ->
            Nothing


root : ExprZip a -> Expr a
root loc =
    case up loc of
        Just parent ->
            root parent

        Nothing ->
            loc.focus


next : ExprZip a -> Maybe (ExprZip a)
next =
    Maybe.oneOf
        [ down
        , right
        , nextHelper
        ]


nextHelper : ExprZip a -> Maybe (ExprZip a)
nextHelper loc =
    case up loc of
        Just parent ->
            case right parent of
                Just r ->
                    Just r

                Nothing ->
                    nextHelper parent

        Nothing ->
            Nothing
