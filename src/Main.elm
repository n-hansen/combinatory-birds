module Main exposing (..)

import Browser
import Combinators exposing (..)
import Html as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h6
        , input
        , pre
        , span
        , table
        , tbody
        , td
        , text
        , textarea
        , tr
        )
import Html.Attributes as Attr exposing (class, value)
import Html.Events exposing (onClick, onInput)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing (Problem(..))
import Result
import Result.Extra as Result
import Set exposing (Set)
import Tuple



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type ParseState a
    = ShowingError ParseError
    | ShowingSuccessfulParse a
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
    case ( msg, model.app ) of
        ( StartEdit, Editing _ ) ->
            model

        ( StartEdit, Halted _ ) ->
            { model
                | app =
                    Editing
                        { program =
                            parseExpr model.session.programInput
                                |> Result.unpack
                                    ShowingError
                                    ShowingSuccessfulParse
                        , rules =
                            parseRuleset model.session.rulesInput
                                |> Result.unpack
                                    ShowingError
                                    ShowingSuccessfulParse
                        }
            }

        ( FinishEdit, Editing editData ) ->
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
                                , history = [ prog ]
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

        ( FinishEdit, _ ) ->
            model

        ( ChangeProgramInput newContent, Editing editData ) ->
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
                                    | program = ShowingSuccessfulParse expr
                                }
                    }

                Err err ->
                    nextModel

        ( ChangeProgramInput _, _ ) ->
            model

        ( ChangeRulesInput newContent, Editing editData ) ->
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
                                    | rules = ShowingSuccessfulParse rules
                                }
                    }

                Err err ->
                    nextModel

        ( ChangeRulesInput _, _ ) ->
            model

        ( StepRules, Halted haltedData ) ->
            case applyRulesOnce haltedData.rules haltedData.currentState of
                Just newState ->
                    { model
                        | app =
                            Halted
                                { haltedData
                                    | currentState = newState
                                    , history = newState :: haltedData.history
                                }
                    }

                Nothing ->
                    model

        ( StepRules, _ ) ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] <|
        case model.app of
            Editing { program, rules } ->
                [ heading "Rewrite rules"
                , div [ class "row" ]
                    [ div [ class "col-md" ] [ rulesInput model.session.rulesInput ]
                    , div [ class "col-md" ] [ rulesView rules ]
                    ]
                , heading "Expression to evaluate"
                , div [ class "row" ]
                    [ div [ class "col-md d-flex" ]
                        [ programInput model.session.programInput
                        , button
                            [ class "btn btn-primary ml-1"
                            , onClick FinishEdit
                            ]
                            [ text "Go" ]
                        ]
                    , div [ class "col-md" ] [ programView program ]
                    ]
                ]

            Halted { initialProgram, rules, currentState, history } ->
                List.singleton <|
                    div [ class "row" ] <|
                        [ div [ class "col-md" ]
                            [ heading "Rewrite rules"
                            , rulesView <| ShowingSuccessfulParse rules
                            , heading "Initial expression"
                            , div [ class "pl-3" ] [ exprView initialProgram ]
                            , heading "Current expression"
                            , div [ class "pl-3" ] [ exprView currentState ]
                            , div [ class "buttonRow mt-3" ]
                                [ button
                                    [ class "btn btn-light border border-secondary"
                                    , onClick StartEdit
                                    ]
                                    [ text "Edit" ]
                                , button
                                    [ class "btn btn-primary"
                                    , onClick StepRules
                                    ]
                                    [ text "Step" ]
                                ]
                            ]
                        , div [ class "col-md" ]
                            [ heading "History"
                            , historyView history
                            ]
                        ]


heading : String -> Html Msg
heading =
    text >> List.singleton >> h6 [ class "mt-2" ]


rulesInput : String -> Html Msg
rulesInput input =
    textarea
        [ Attr.rows 12
        , onInput ChangeRulesInput
        ]
        [ text input ]


programInput : String -> Html Msg
programInput input =
    Html.input
        [ Attr.type_ "text"
        , onInput ChangeProgramInput
        , value input
        , class "flex-grow-1"
        ]
        []


rulesView : ParseState (List RewriteRule) -> Html Msg
rulesView ps =
    div [ class "rulesView table-responsive" ]
        [ case ps of
            InitialParseState ->
                text "Enter some rules!"

            ShowingError err ->
                parseErrorView err

            ShowingSuccessfulParse rules ->
                table [ class "table mb-0" ]
                    [ rules
                        |> List.map
                            (\{ pattern, replacement } ->
                                tr []
                                    [ td [ class "pattern pr-1" ]
                                        [ exprView pattern ]
                                    , td [ class "arr px-0" ]
                                        [ text "⇒" ]
                                    , td [ class "replacement pl-1" ]
                                        [ exprView replacement ]
                                    ]
                            )
                        |> tbody []
                    ]
        ]


programView : ParseState PlainExpr -> Html Msg
programView ps =
    div [] <|
        case ps of
            InitialParseState ->
                []

            ShowingSuccessfulParse expr ->
                [ exprView expr ]

            ShowingError err ->
                [ parseErrorView err ]


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
        |> div [ class "expr" ]


parseErrorView : ParseError -> Html Msg
parseErrorView ( input, err ) =
    if input == "" then
        div [ class "parseError" ] [ span [ class "msg" ] [ text "You need to provide some input!" ] ]

    else
        let
            lines =
                String.lines input
        in
        err
            -- TODO maybe get smarter about handling multiple failures
            |> List.take 1
            |> List.map
                (\{ row, col, problem } ->
                    let
                        ( before, after ) =
                            List.splitAt row lines
                                |> Tuple.mapBoth (String.join "\n") (String.join "\n")
                    in
                    pre []
                        [ div [ class "msg" ]
                            [ text <|
                                "Parse Error: "
                                    ++ (case problem of
                                            ExpectingVariable ->
                                                "Expected a term or variable, perhaps try a letter?"

                                            ExpectingSymbol sym ->
                                                "Expected " ++ sym

                                            Problem p ->
                                                "Encountered problem: " ++ p

                                            UnexpectedChar ->
                                                "Unexpected char"

                                            Expecting expect ->
                                                "Expected " ++ expect

                                            ExpectingEnd ->
                                                if
                                                    (input |> String.indexes "(" |> List.length)
                                                        /= (input |> String.indexes ")" |> List.length)
                                                then
                                                    "Unbalanced )"

                                                else
                                                    "Expected end (probably an illegal character)."

                                            BadRepeat ->
                                                "`Bad repeat'"

                                            _ ->
                                                "A weird problem occured, sorry."
                                       )
                            ]
                        , div [ class "ctx" ]
                            [ text before ]
                        , div [ class "colPtr" ]
                            [ span [ class "squiggle" ]
                                [ text <|
                                    String.repeat (col - 1) "~"
                                ]
                            , span [ class "arr" ] [ text "^" ]
                            ]
                        , div [ class "ctx" ] [ text after ]
                        ]
                )
            |> div [ class "parseError" ]


historyView : List (Expr a) -> Html Msg
historyView hist =
    hist
        |> List.foldl
            (\expr rendered ->
                exprView expr :: rendered
            )
            []
        |> Html.ul [ class "list-unstyled historyView" ]
