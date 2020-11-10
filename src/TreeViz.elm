module TreeViz exposing
    ( PlacedEdge
    , PlacedNode
    , RenderConfig
    , Tree(..)
    , TreeLayout
    , defaultRenderConfig
    , layoutTree
    , renderSvg
    )

import Debug
import Dict exposing (Dict)
import Either exposing (Either(..))
import Html exposing (Html)
import Maybe
import Maybe.Extra as Maybe
import String
import Svg exposing (Svg, circle, line, rect, svg, text, text_)
import Svg.Attributes as Attr
import Tuple



-- Tree layouts using the approach of Reingold and Tilford in "Tidier Drawings of Trees" (1981)


type Tree
    = Node Tree Tree
    | Leaf String


type AnnotatedTree
    = ANode { offset : Float, id : Int } AnnotatedTree AnnotatedTree
    | ALeaf { id : Int } String


type alias AnnotationContext =
    { threads : Dict Int ( AnnotatedTree, Float )
    , nextId : Int
    }


atId : AnnotatedTree -> Int
atId t =
    case t of
        ANode { id } _ _ ->
            id

        ALeaf { id } _ ->
            id


type alias TreeLayout =
    { nodes : List PlacedNode
    , edges : List PlacedEdge
    }


type alias PlacedNode =
    { x : Float
    , y : Float
    , str : Maybe String
    }


type alias PlacedEdge =
    { start : { x : Float, y : Float }
    , end : { x : Float, y : Float }
    }


type alias ContourResult =
    { dangling : Maybe (Either ( AnnotatedTree, Float ) ( AnnotatedTree, Float ))
    , maxOverlap : Float
    }


contourLeft : Dict Int ( AnnotatedTree, Float ) -> AnnotatedTree -> Maybe ( AnnotatedTree, Float )
contourLeft threads t =
    case t of
        ANode { offset } l _ ->
            Just ( l, offset )

        ALeaf { id } _ ->
            Dict.get id threads


contourRight : Dict Int ( AnnotatedTree, Float ) -> AnnotatedTree -> Maybe ( AnnotatedTree, Float )
contourRight threads t =
    case t of
        ANode { offset } _ r ->
            Just ( r, offset )

        ALeaf { id } _ ->
            Dict.get id threads


contourLeftEnd : Dict Int ( AnnotatedTree, Float ) -> AnnotatedTree -> ( AnnotatedTree, Float )
contourLeftEnd threads =
    let
        helper os t =
            case contourLeft threads t of
                Just ( l, o ) ->
                    helper (o + os) l

                Nothing ->
                    ( t, os )
    in
    helper 0.0


contourRightEnd : Dict Int ( AnnotatedTree, Float ) -> AnnotatedTree -> ( AnnotatedTree, Float )
contourRightEnd threads =
    let
        helper os t =
            case contourRight threads t of
                Just ( r, o ) ->
                    helper (o + os) r

                Nothing ->
                    ( t, os )
    in
    helper 0.0


innerContour : Dict Int ( AnnotatedTree, Float ) -> AnnotatedTree -> AnnotatedTree -> ContourResult
innerContour threads l r =
    innerContourHelper threads l r 0.0 0.0 0.0


innerContourHelper :
    Dict Int ( AnnotatedTree, Float )
    -> AnnotatedTree
    -> AnnotatedTree
    -> Float
    -> Float
    -> Float
    -> ContourResult
innerContourHelper threads leftInner rightInner leftOffset rightOffset maxOverlap =
    case
        ( contourRight threads leftInner
        , contourLeft threads rightInner
        )
    of
        ( Just ( li, lio ), Just ( ri, rio ) ) ->
            innerContourHelper
                threads
                li
                ri
                (leftOffset + lio)
                (rightOffset + rio)
                (max maxOverlap <| leftOffset + rightOffset + lio + rio)

        ( Just ( li, lio ), Nothing ) ->
            { maxOverlap = maxOverlap
            , dangling = Just <| Left ( li, leftOffset + lio )
            }

        ( Nothing, Just ( ri, rio ) ) ->
            { maxOverlap = maxOverlap
            , dangling = Just <| Right ( ri, rightOffset + rio )
            }

        ( Nothing, Nothing ) ->
            { maxOverlap = maxOverlap
            , dangling = Nothing
            }


annotateTree : Tree -> AnnotatedTree
annotateTree =
    annotateTreeHelper
        { threads = Dict.empty
        , nextId = 1
        }
        >> Tuple.first


annotateTreeHelper : AnnotationContext -> Tree -> ( AnnotatedTree, AnnotationContext )
annotateTreeHelper ctx t =
    case t of
        Leaf s ->
            ( ALeaf { id = ctx.nextId } s
            , { ctx
                | nextId = ctx.nextId + 1
              }
            )

        Node l r ->
            let
                ( la, lctx ) =
                    annotateTreeHelper ctx l

                ( ra, rctx ) =
                    annotateTreeHelper lctx r

                { maxOverlap, dangling } =
                    innerContour rctx.threads la ra

                offset =
                    (maxOverlap + 1) / 2

                newThreads =
                    case dangling of
                        Nothing ->
                            rctx.threads

                        Just (Left ( lnext, loffset )) ->
                            let
                                ( toThread, roffset ) =
                                    contourRightEnd rctx.threads ra
                            in
                            rctx.threads
                                |> Dict.insert (atId toThread)
                                    ( lnext, offset + roffset - loffset )

                        Just (Right ( rnext, roffset )) ->
                            let
                                ( toThread, loffset ) =
                                    contourLeftEnd rctx.threads la
                            in
                            rctx.threads
                                |> Dict.insert (atId toThread)
                                    ( rnext, offset + loffset - roffset )
            in
            ( ANode { offset = offset, id = rctx.nextId } la ra
            , { rctx
                | threads = newThreads
                , nextId = rctx.nextId + 1
              }
            )


placeElements : AnnotatedTree -> TreeLayout
placeElements t =
    placeElementsHelper [ ( t, 0.0, 0.0 ) ] Dict.empty []


placeElementsHelper :
    List ( AnnotatedTree, Float, Float )
    -> Dict Int PlacedNode
    -> List ( Int, Int )
    -> TreeLayout
placeElementsHelper pending nodes edges =
    case pending of
        ( ALeaf { id } s, x, y ) :: rest ->
            placeElementsHelper
                rest
                (Dict.insert id { x = x, y = y, str = Just s } nodes)
                edges

        ( ANode { id, offset } l r, x, y ) :: rest ->
            placeElementsHelper
                (( l, x - offset, y + 1 )
                    :: ( r, x + offset, y + 1 )
                    :: rest
                )
                (Dict.insert id { x = x, y = y, str = Nothing } nodes)
                (( id, atId l )
                    :: ( id, atId r )
                    :: edges
                )

        [] ->
            { nodes = Dict.values nodes
            , edges =
                edges
                    |> List.filterMap
                        (\( fromId, toId ) ->
                            Maybe.map2
                                (\from to ->
                                    { start = { x = from.x, y = from.y }
                                    , end = { x = to.x, y = to.y }
                                    }
                                )
                                (Dict.get fromId nodes)
                                (Dict.get toId nodes)
                        )
            }


layoutTree : Tree -> TreeLayout
layoutTree =
    annotateTree >> placeElements


type alias RenderContext =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type alias RenderConfig =
    { xScale : Float
    , yScale : Float
    , viewportPad : Float
    , viewportScale : Float
    , nodeRadius : Float
    , leafHeight : Float
    , leafWidth : Float
    }


defaultRenderConfig : RenderConfig
defaultRenderConfig =
    { xScale = 30
    , yScale = 20
    , viewportPad = 15
    , viewportScale = 0.8
    , nodeRadius = 4
    , leafHeight = 15
    , leafWidth = 15
    }


renderSvg : RenderConfig -> TreeLayout -> Html msg
renderSvg { xScale, yScale, viewportPad, viewportScale, nodeRadius, leafHeight, leafWidth } { nodes, edges } =
    let
        ( renderedNodes, { minX, maxX, minY, maxY } ) =
            nodes
                |> List.foldl
                    (\{ str, x, y } ( elems, ctx ) ->
                        let
                            elem =
                                case str of
                                    Nothing ->
                                        circle
                                            [ Attr.cx << String.fromFloat <| x * xScale
                                            , Attr.cy << String.fromFloat <| y * yScale
                                            , Attr.r << String.fromFloat <| nodeRadius
                                            , Attr.class "node"
                                            ]
                                            []

                                    Just lbl ->
                                        Svg.g
                                            [ Attr.class "leaf" ]
                                            [ rect
                                                [ Attr.x
                                                    << String.fromFloat
                                                  <|
                                                    x
                                                        * xScale
                                                        - leafWidth
                                                        / 2
                                                , Attr.y
                                                    << String.fromFloat
                                                  <|
                                                    y
                                                        * yScale
                                                        - leafHeight
                                                        / 2
                                                , Attr.width
                                                    << String.fromFloat
                                                  <|
                                                    leafWidth
                                                , Attr.height
                                                    << String.fromFloat
                                                  <|
                                                    leafHeight
                                                ]
                                                []
                                            , text_
                                                [ Attr.x << String.fromFloat <| x * xScale
                                                , Attr.y << String.fromFloat <| y * yScale
                                                ]
                                                [ text lbl ]
                                            ]
                        in
                        ( elem :: elems
                        , { minX = min ctx.minX <| x * xScale
                          , maxX = max ctx.maxX <| x * xScale
                          , minY = min ctx.minY <| y * yScale
                          , maxY = max ctx.maxY <| y * yScale
                          }
                        )
                    )
                    ( [], RenderContext 0.0 0.0 0.0 0.0 )

        renderedEdges =
            edges
                |> List.map
                    (\{ start, end } ->
                        line
                            [ Attr.x1 << String.fromFloat <| start.x * xScale
                            , Attr.x2 << String.fromFloat <| end.x * xScale
                            , Attr.y1 << String.fromFloat <| start.y * yScale
                            , Attr.y2 << String.fromFloat <| end.y * yScale
                            , Attr.class "edge"
                            ]
                            []
                    )

        width =
            maxX - minX + 2 * viewportPad

        height =
            maxY - minY + 2 * viewportPad
    in
    svg
        [ Attr.viewBox <|
            String.fromFloat (minX - viewportPad)
                ++ " "
                ++ String.fromFloat (minY - viewportPad)
                ++ " "
                ++ String.fromFloat width
                ++ " "
                ++ String.fromFloat height
        , Attr.width <| String.fromFloat (width * viewportScale)
        , Attr.height <| String.fromFloat (height * viewportScale)
        ]
        (renderedEdges ++ renderedNodes)
