module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
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
import Html.Attributes as Attr exposing (class, classList, value)
import Html.Events exposing (onClick, onInput)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing (Problem(..))
import Platform exposing (Program)
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub
import Result
import Result.Extra as Result
import Set exposing (Set)
import Task
import Tuple



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \msg mdl -> ( update msg mdl, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }



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
    | Halted ExecutionData
    | Running Int ExecutionData


type alias ExecutionData =
    { initialProgram : PlainExpr
    , rules : List ( RewriteRule, RuleDirection )
    , currentState : PlainExpr
    , history : List RewrittenExpr
    }


type RuleDirection
    = Forward
    | Reverse


flipDirection : RuleDirection -> RuleDirection
flipDirection d =
    case d of
        Forward ->
            Reverse

        Reverse ->
            Forward


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
    | ReverseRule Int
    | StartRunning
    | TickRunning
    | StopRunning


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.app ) of
        ( StartEdit, Editing _ ) ->
            model

        ( StartEdit, _ ) ->
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
                                , rules =
                                    rules
                                        |> List.map (\r -> ( r, Forward ))
                                , currentState = prog
                                , history = [ mapExpr (always emptyRewriteData) prog ]
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
            case stepRules haltedData of
                Just newData ->
                    { model
                        | app = Halted newData
                    }

                Nothing ->
                    model

        ( StepRules, _ ) ->
            model

        ( ReverseRule ix, Halted haltedData ) ->
            { model
                | app =
                    Halted
                        { haltedData
                            | rules =
                                haltedData.rules
                                    |> List.updateAt ix
                                        (Tuple.mapSecond flipDirection)
                        }
            }

        ( ReverseRule ix, Running stepsLeft runningData ) ->
            { model
                | app =
                    Running stepsLeft
                        { runningData
                            | rules =
                                runningData.rules
                                    |> List.updateAt ix (Tuple.mapSecond flipDirection)
                        }
            }

        ( ReverseRule _, _ ) ->
            model

        ( StartRunning, Halted haltedData ) ->
            { model
                | app = Running 50 haltedData
            }

        ( StartRunning, _ ) ->
            model

        ( StopRunning, Running _ runningData ) ->
            { model
                | app = Halted runningData
            }

        ( StopRunning, _ ) ->
            model

        ( TickRunning, Running stepsLeft runningData ) ->
            case stepRules runningData of
                Just newData ->
                    { model
                        | app =
                            if stepsLeft > 0 then
                                Running (stepsLeft - 1) newData

                            else
                                Halted newData
                    }

                Nothing ->
                    { model
                        | app = Halted runningData
                    }

        ( TickRunning, _ ) ->
            model


stepRules : ExecutionData -> Maybe ExecutionData
stepRules data =
    let
        varSuffix =
            data.history |> List.length |> String.fromInt
    in
    data.history
        |> List.head
        |> Maybe.andThen
            (applyRulesOnce <|
                RuleList <|
                    List.map
                        (\( r, dir ) ->
                            mangleUnboundVars varSuffix <|
                                case dir of
                                    Forward ->
                                        r

                                    Reverse ->
                                        reverseRule r
                        )
                        data.rules
            )
        |> Maybe.map
            (\( taggedOldState, newState ) ->
                { data
                    | currentState = mapExpr (always ()) newState
                    , history =
                        newState
                            :: taggedOldState
                            :: List.drop 1 data.history
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.app of
        Running _ _ ->
            onAnimationFrame (always TickRunning)

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] <|
        case model.app of
            Editing { program, rules } ->
                [ heading "Rewrite rules"
                , div [ class "row" ]
                    [ div [ class "col-md" ] [ rulesInput model.session.rulesInput ]
                    , div [ class "col-md" ] [ editingRulesView rules ]
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

            Halted haltedData ->
                [ executionView haltedData <|
                    div [ class "buttonRow mt-3" ]
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
                        , button
                            [ class "btn btn-primary"
                            , onClick StartRunning
                            ]
                            [ text "Run" ]
                        ]
                ]

            Running _ runningData ->
                [ executionView runningData <|
                    div [ class "buttonRow mt-3" ]
                        [ button
                            [ class "btn btn-light border border-secondary"
                            , onClick StartEdit
                            ]
                            [ text "Edit" ]
                        , button
                            [ class "btn btn-danger"
                            , onClick StopRunning
                            ]
                            [ text "Stop" ]
                        ]
                ]


heading : String -> Html Msg
heading =
    text >> List.singleton >> h6 [ class "mt-2" ]


executionView : ExecutionData -> Html Msg -> Html Msg
executionView { initialProgram, rules, currentState, history } buttons =
    div [ class "row" ] <|
        [ div [ class "col-md" ]
            [ heading "Rewrite rules"
            , if List.isEmpty rules then
                div [ class "font-italic" ]
                    [ text "There are no rewrite rules, which may be a bit dull." ]

              else
                executionRulesView rules
            , heading "Initial expression"
            , div [ class "pl-3" ] [ plainExprView initialProgram ]
            , heading "Current expression"
            , div [ class "pl-3" ] [ plainExprView currentState ]
            , buttons
            ]
        , div [ class "col-md" ]
            [ heading "History"
            , historyView history
            ]
        ]


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


editingRulesView : ParseState (List RewriteRule) -> Html Msg
editingRulesView ps =
    case ps of
        InitialParseState ->
            div [] [ text "Enter some rules!" ]

        ShowingError err ->
            parseErrorView err

        ShowingSuccessfulParse rules ->
            rulesViewHelper Nothing Nothing <|
                List.map (\r -> ( r, Forward )) rules


executionRulesView : List ( RewriteRule, RuleDirection ) -> Html Msg
executionRulesView =
    rulesViewHelper
        (Just <|
            \ix ->
                td [] <| List.singleton <|
                div
                    [ class <|
                        "rule-swatch-"
                            ++ String.fromInt (modBy 20 ix)
                    ]
                    []
        )
        (Just <|
            \ix ->
                td [] <| List.singleton <|
                div
                    [ class "btn-group btn-group-sm" ]
                    [ button
                        [ class "btn btn-dark"
                        , onClick <| ReverseRule ix
                        ]
                        [ text "⇄" ]
                    ]
        )


rulesViewHelper :
    Maybe (Int -> Html Msg)
    -> Maybe (Int -> Html Msg)
    -> List ( RewriteRule, RuleDirection )
    -> Html Msg
rulesViewHelper rowPrefix rowSuffix rules =
    div [ class "rulesView table-responsive" ]
        [ table [ class "table table-sm mb-0" ]
            [ rules
                |> List.indexedMap
                    (\ix ( { pattern, replacement }, dir ) ->
                        tr [] <|
                            Maybe.values
                                [ rowPrefix
                                    |> Maybe.andMap (Just ix)
                                , Just <|
                                    td [ class "pr-1" ]
                                        [ plainExprView pattern ]
                                , Just <|
                                    td [ class "arr px-0" ] <|
                                        case dir of
                                            Forward ->
                                                [ text "⇒" ]

                                            Reverse ->
                                                [ text "⇐" ]
                                , Just <|
                                    td [ class "pl-1" ]
                                        [ plainExprView replacement ]
                                , rowSuffix
                                    |> Maybe.andMap (Just ix)
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
                [ plainExprView expr ]

            ShowingError err ->
                [ parseErrorView err ]


flatTreeRenderer : (a -> List (Attribute Msg)) -> Renderer a (Html Msg)
flatTreeRenderer attrs =
    { term =
        \a t ->
            div
                (class "term" :: attrs a)
                [ text t ]
    , freeVar =
        \a v ->
            div
                (class "freeVar" :: attrs a)
                [ text v ]
    , appl =
        \_ a x y ->
            div
                (class "appl" :: attrs a)
                [ x, y ]
    }


plainExprView : Expr a -> Html Msg
plainExprView expr =
    expr
        |> renderExpr (flatTreeRenderer (always []))
        |> List.singleton
        |> div [ class "expr" ]


rewrittenExprView : RewrittenExpr -> Html Msg
rewrittenExprView expr =
    expr
        |> renderExpr
            (flatTreeRenderer <|
                \{ rewrittenFrom, rewrittenTo } ->
                    Maybe.values
                        [ rewrittenFrom
                            |> Maybe.map
                                (modBy 20
                                    >> String.fromInt
                                    >> (\s -> "rewrite-from-" ++ s)
                                    >> class
                                )
                        , rewrittenTo
                            |> Maybe.map
                                (modBy 20
                                    >> String.fromInt
                                    >> (\s -> "rewrite-to-" ++ s)
                                    >> class
                                )
                        ]
            )
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


historyView : List RewrittenExpr -> Html Msg
historyView hist =
    hist
        |> List.foldl
            (\expr rendered ->
                rewrittenExprView expr :: rendered
            )
            []
        |> Html.ul [ class "list-unstyled historyView" ]
