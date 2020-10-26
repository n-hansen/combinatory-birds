module Main exposing (..)

import Browser
import Combinators exposing (..)
import Html as Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes as Attr exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Set exposing (Set)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type ParseState a
    = ShowingError String
    | ShowingLastSuccessfulParse a
    | InitialParseState


type alias SessionData =
    { programInput : String
    , rulesInput : String
    }


type ApplicationState
    = Editing
        { program : ParseState PlainExpr
        , rules : ParseState (List RewriteRule)
        }
    | Halted
        { initialProgram : PlainExpr
        , rules : List RewriteRule
        , currentState : PlainExpr
        , history : List PlainExpr

        -- , loopDetection : Set String
        }



-- | Running { currentState : PlainExpr
--           , history : List PlainExpr
--           , stepsLeft : Int
--           , loopDetection : Set String
--           }


type alias Model =
    { session : SessionData
    , app : ApplicationState
    }


init : Model
init =
    { session =
        { programInput = ""
        , rulesInput = ""
        }
    , app =
        Editing
            { program = InitialParseState
            , rules = InitialParseState
            }
    }



-- UPDATE


type Msg
    = ChangeProgramInput String
    | ChangeRulesInput String
    | StartEdit
    | FinishEdit
    | StepRules


update : Msg -> Model -> Model
update msg model =
    case (msg, model.app) of
        (StartEdit, Editing _) ->
                model

        (StartEdit, Halted _) ->
                { model
                    | app =
                        Editing
                                { program =
                                    parseExpr model.session.programInput
                                        |> Result.unpack
                                            ShowingError
                                            ShowingLastSuccessfulParse
                                , rules =
                                    parseRuleset model.session.rulesInput
                                        |> Result.unpack
                                            ShowingError
                                            ShowingLastSuccessfulParse
                                }
                    }

        (FinishEdit, Editing editData) ->
            case
                ( parseExpr model.session.programInput
                , parseRuleset model.session.rulesInput
                )
            of
                ( Ok prog, Ok rules ) ->
                    { model
                        | app =
                            Halted
                                { initialProgram = prog
                                , rules = rules
                                , currentState = prog
                                , history = []
                                }
                    }

                ( prog, rules ) ->
                    { model
                        | app =
                            Editing
                                { program =
                                    prog
                                        |> Result.error
                                        |> Maybe.unwrap editData.program ShowingError
                                , rules =
                                    rules
                                        |> Result.error
                                        |> Maybe.unwrap editData.rules ShowingError
                                }
                    }

        (FinishEdit, _) ->
            model

        (ChangeProgramInput newContent, Editing editData) ->
                    let
                        sesh =
                            model.session

                        nextModel =
                            { model | session = { sesh | programInput = newContent } }
                    in
                    case parseExpr newContent of
                        Ok expr ->
                            { nextModel
                                | app =
                                    Editing
                                        { editData
                                            | program = ShowingLastSuccessfulParse expr
                                        }
                            }

                        Err err ->
                            nextModel

        (ChangeProgramInput _, _) ->
                    model

        (ChangeRulesInput newContent, Editing editData) ->
                    let
                        sesh =
                            model.session

                        nextModel =
                            { model | session = { sesh | rulesInput = newContent } }
                    in
                    case parseRuleset newContent of
                        Ok rules ->
                            { nextModel
                                | app =
                                    Editing
                                        { editData
                                            | rules = ShowingLastSuccessfulParse rules
                                        }
                            }

                        Err err ->
                            nextModel

        (ChangeRulesInput _, _) ->
                    model

        (StepRules, Halted haltedData) ->
                    case applyRulesOnce haltedData.rules haltedData.currentState of
                        Just newState ->
                            { model
                                | app =
                                    Halted
                                        { haltedData
                                            | currentState = newState
                                            , history =
                                                haltedData.currentState
                                                    :: haltedData.history
                                        }
                            }

                        Nothing ->
                            model

        (StepRules, _) ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    case model.app of
        Editing {program, rules} ->
            div
                [ class "editView" ]
                    [ rulesInput model.session.rulesInput
                    , rulesView rules
                    , programInput model.session.programInput
                    , programView program
                    , button [ onClick FinishEdit ] [ text "Submit" ]
                    ]

        _ ->
            div [] []


rulesInput : String -> Html Msg
rulesInput input =
    textarea
        [ class "rulesInput"
        , Attr.rows 16
        , onInput ChangeRulesInput
        ]
        [ text input ]


programInput : String -> Html Msg
programInput input =
    textarea
        [ class "programInput"
        , Attr.rows 1
        , onInput ChangeProgramInput
        ]
        [ text input ]


rulesView : ParseState (List RewriteRule) -> Html Msg
rulesView ps =
    div [class "rulesView"] <|
    case ps of
        InitialParseState ->
            [text "Enter some rules!"]

        ShowingLastSuccessfulParse expr ->
            [text "some rules"]

        ShowingError err ->
            [div [class "parseError"] [ text err ]]


programView : ParseState PlainExpr -> Html Msg
programView ps =
    div [class "programView"] <|
        case ps of
            InitialParseState ->
                []

            ShowingLastSuccessfulParse expr ->
                [exprView expr]

            ShowingError err ->
                [div [class "parseError"] [text err]]


flatTreeRenderer : Renderer a (Html Msg)
flatTreeRenderer =
    { term =
        \_ t ->
            div
                [ class "term" ]
                [ text t ]
    , freeVar =
        \_ v ->
            div
                [ class "freeVar" ]
                [ text v ]
    , appl =
        \_ _ x y ->
            div
                [ class "appl" ]
                [ x, y ]
    }


exprView : Expr a -> Html Msg
exprView expr =
    expr
        |> renderExpr flatTreeRenderer
        |> List.singleton
        |> div [class "expr"]
