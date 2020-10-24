module Main exposing (..)

import Browser
import Combinators exposing (..)
import Css exposing (..)
import Html as UnstyledHtml
import Html.Styled as Html exposing (Attribute, Html, div, input, text, toUnstyled)
import Html.Styled.Attributes as Attr exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Maybe
import Maybe.Extra as Maybe



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



-- MODEL


type alias Model =
    { textContent : String
    , parsedExpr : Maybe PlainExpr
    , error : Maybe String
    }


init : Model
init =
    { textContent = ""
    , parsedExpr = Nothing
    , error = Nothing
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change "" ->
            init

        Change newContent ->
            case parseExpr newContent of
                Ok expr ->
                    { model
                        | textContent = newContent
                        , parsedExpr = Just expr
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | textContent = newContent
                        , error = Just err
                    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.textContent, onInput Change ] []
        , model.parsedExpr
            |> Maybe.map (renderExpr flatTreeRenderer)
            |> Maybe.toList
            |> div [ css [ displayFlex ] ]
        ]


flatTreeRenderer : Renderer () (Html Msg)
flatTreeRenderer =
    { term =
        \_ t ->
            div
                [ css
                    [ padding <| em 0.1
                    ]
                ]
                [ text t ]
    , freeVar =
        \_ v ->
            div
                [ css
                    [ padding <| em 0.1
                    , fontStyle italic
                    ]
                ]
                [ text v ]
    , appl =
        \_ _ x y ->
            div
                [ css
                    [ displayFlex
                    , flexDirection row
                    , alignItems baseline
                    , border3 (px 1) solid (rgb 0 0 0)
                    , padding <| em 0.3
                    , margin2 (px 0) (em 0.1)
                    ]
                ]
                [ x, y ]
    }
