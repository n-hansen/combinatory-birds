module Main exposing (..)


import Browser
import Css exposing (..)
import Html as UnstyledHtml
import Html.Styled as Html
import Html.Styled exposing (Html, Attribute, div, input, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Maybe
import Maybe.Extra as Maybe
import Combinators exposing (..)


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
    Change "" -> init

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
        |> Maybe.map renderExpr
        |> Maybe.toList
        |> div [ css [displayFlex] ]
    ]


renderExpr : PlainExpr -> Html Msg
renderExpr expr =
    case expr of
        Term () t ->
            div
            [ css
                  [ padding <| em 0.1

                  ]
            ]
            [ text t ]

        Appl () x y ->
            div
            [ css
                  [ displayFlex
                  , flexDirection row
                  , alignItems center
                  , border3 (px 1) solid (rgb 0 0 0)
                  , padding <| em 0.3
                  , margin2 (px 0) (em 0.1)
                  ]
            ]
            [ renderExpr x, renderExpr y ]
