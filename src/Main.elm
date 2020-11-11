module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Combinators exposing (..)
import Combinators.Search exposing (searchForMatch)
import Either exposing (Either(..))
import Html as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h1
        , h5
        , h6
        , input
        , li
        , nav
        , pre
        , span
        , table
        , tbody
        , td
        , text
        , textarea
        , tr
        , ul
        )
import Html.Attributes as Attr exposing (class, classList, title, value)
import Html.Events exposing (onClick, onInput)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing (Problem(..))
import Platform exposing (Program)
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub
import Process
import Result
import Result.Extra as Result
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgAttr
import Task
import TreeViz
import Tuple



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type ParseState a
    = ShowingError ParseError
    | ShowingSuccessfulParse a
    | InitialParseState


type DisplayStyle
    = SymbolString
    | BorderedTree
    | PrettyTree


type alias SessionData =
    { programInput : String
    , rulesInput : String
    , searchGoalInput : String
    , settingsOpen : Bool
    , displayStyle : DisplayStyle
    , searchDepth : Int
    , searchMaxExprLength : Int
    }


type ApplicationState
    = Editing
        { program : ParseState PlainExpr
        , rules : ParseState (List RewriteRule)
        , searchGoal : ParseState (Either RewriteRule PlainExpr)
        }
    | Halted ExecutionData
    | Running Int ExecutionData


type alias ExecutionData =
    { initialProgram : PlainExpr
    , rules : List ( RewriteRule, RuleDirection )
    , searchGoal : Maybe PlainExpr
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
        , searchGoalInput = ""
        , settingsOpen = False
        , displayStyle = SymbolString
        , searchDepth = 10
        , searchMaxExprLength = 20
        }
    , app =
        Editing
            { program = InitialParseState
            , rules = InitialParseState
            , searchGoal = InitialParseState
            }
    }



-- UPDATE


type Msg
    = ChangeProgramInput String
    | ChangeRulesInput String
    | ChangeSearchGoalInput String
    | ToggleSettings
    | SetDisplayStyle DisplayStyle
    | StartEdit
    | FinishEdit
    | StepRules
    | StepSingleRule RuleDirection Int
    | ReverseRule Int
    | StartRunning
    | TickRunning
    | StopRunning
    | StartSearch PlainExpr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.app ) of
        ( ToggleSettings, _ ) ->
            let
                sesh =
                    model.session
            in
            noCmd { model | session = { sesh | settingsOpen = not sesh.settingsOpen } }

        ( SetDisplayStyle style, _ ) ->
            let
                sesh =
                    model.session
            in
            noCmd { model | session = { sesh | displayStyle = style } }

        ( StartEdit, Editing _ ) ->
            noCmd model

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
                        , searchGoal =
                            parseSearchGoal model.session.searchGoalInput
                                |> Result.unpack
                                    ShowingError
                                    ShowingSuccessfulParse
                        }
            }
                |> noCmd

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
                                , searchGoal = realizeSearchGoal prog model.session.searchGoalInput
                                }
                    }
                        |> noCmd

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
                                , searchGoal =
                                    if model.session.searchGoalInput == "" then
                                        InitialParseState

                                    else
                                        parseSearchGoal model.session.searchGoalInput
                                            |> Result.unpack
                                                ShowingError
                                                ShowingSuccessfulParse
                                }
                    }
                        |> noCmd

        ( FinishEdit, _ ) ->
            noCmd model

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
                        |> noCmd

                Err err ->
                    noCmd nextModel

        ( ChangeProgramInput _, _ ) ->
            noCmd model

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
                        |> noCmd

                Err err ->
                    noCmd nextModel

        ( ChangeRulesInput _, _ ) ->
            noCmd model

        ( ChangeSearchGoalInput newContent, Editing editData ) ->
            let
                sesh =
                    model.session

                nextModel =
                    { model | session = { sesh | searchGoalInput = newContent } }

                nextGoal =
                    if newContent == "" then
                        InitialParseState

                    else
                        case parseSearchGoal newContent of
                            Ok goal ->
                                ShowingSuccessfulParse goal

                            Err err ->
                                editData.searchGoal
            in
            { nextModel
                | app =
                    Editing { editData | searchGoal = nextGoal }
            }
                |> noCmd

        ( ChangeSearchGoalInput _, _ ) ->
            noCmd model

        ( StepRules, Halted haltedData ) ->
            case stepRules haltedData of
                Just newData ->
                    noCmd
                        { model
                            | app = Halted newData
                        }

                Nothing ->
                    noCmd model

        ( StepRules, _ ) ->
            noCmd model

        ( StepSingleRule dir ix, Halted haltedData ) ->
            let
                varSuffix =
                    haltedData.history |> List.length |> String.fromInt

                ruleM =
                    haltedData.rules
                        |> List.getAt ix
                        |> Maybe.map
                            (case dir of
                                Forward ->
                                    Tuple.first
                                        >> mangleUnboundVars varSuffix
                                        >> makeSingletonRule ix

                                Reverse ->
                                    Tuple.first
                                        >> reverseRule
                                        >> mangleUnboundVars varSuffix
                                        >> makeSingletonRule ix
                            )

                progM =
                    List.head haltedData.history
            in
            case Maybe.andThen2 applyRulesOnce ruleM progM of
                Just ( taggedOldState, newState ) ->
                    { model
                        | app =
                            Halted
                                { haltedData
                                    | currentState = mapExpr (always ()) newState
                                    , history =
                                        newState
                                            :: taggedOldState
                                            :: List.drop 1 haltedData.history
                                }
                    }
                        |> noCmd

                Nothing ->
                    noCmd model

        ( StepSingleRule _ _, _ ) ->
            noCmd model

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
                |> noCmd

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
                |> noCmd

        ( ReverseRule _, _ ) ->
            noCmd model

        ( StartRunning, Halted haltedData ) ->
            { model
                | app = Running 50 haltedData
            }
                |> noCmd

        ( StartRunning, _ ) ->
            noCmd model

        ( StartSearch goal, Halted haltedData ) ->
            case
                haltedData.history
                    |> List.head
                    |> Maybe.andThen
                        (searchForMatch
                            model.session.searchDepth
                            model.session.searchMaxExprLength
                            (buildSearchRules haltedData.rules)
                            goal
                        )
            of
                Just (e :: es) ->
                    { model
                        | app =
                            Halted
                                { haltedData
                                    | currentState =
                                        mapExpr (always ()) e
                                    , history =
                                        (e :: es)
                                            ++ List.drop 1 haltedData.history
                                }
                    }
                        |> noCmd

                _ ->
                    noCmd model

        ( StartSearch _, _ ) ->
            noCmd model

        ( StopRunning, Running _ runningData ) ->
            { model
                | app = Halted runningData
            }
                |> noCmd

        ( StopRunning, _ ) ->
            noCmd model

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
                        |> noCmd

                Nothing ->
                    { model
                        | app = Halted runningData
                    }
                        |> noCmd

        ( TickRunning, _ ) ->
            noCmd model


noCmd : Model -> ( Model, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


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
                makeRuleList <|
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


parseSearchGoal : String -> Result ParseError (Either RewriteRule PlainExpr)
parseSearchGoal input =
    if String.contains "=" input || String.contains "->" input then
        parseRewriteRule input
            |> Result.map Left

    else
        parseExpr input
            |> Result.map Right


realizeSearchGoal : PlainExpr -> String -> Maybe PlainExpr
realizeSearchGoal toMatch patternInput =
    parseSearchGoal patternInput
        |> Result.toMaybe
        |> Maybe.andThen
            (Either.unpack
                (\rule ->
                    tryRulePlain rule toMatch
                )
                Just
            )


buildSearchRules : List ( RewriteRule, a ) -> Ruleset
buildSearchRules rules =
    rules
        |> List.indexedMap
            (\ix ( rule, _ ) ->
                [ { ix = ix, rule = rule }
                , { ix = ix, rule = reverseRule rule }
                ]
            )
        |> List.concat



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
    div []
        [ nav []
            [ ul [ class "nav" ]
                [ li [ class "nav-item" ] [ h1 [ class "px-2" ] [ text "Combinatory Birds" ] ]
                , li [ class "nav-item ml-auto" ]
                    [ button
                        [ class "btn btn-light border my-2 mr-3 ml-auto align-self-end"
                        , onClick ToggleSettings
                        ]
                        [ featherIcon "settings" ]
                    ]
                ]
            ]
        , div [ class "container-fluid" ] <|
            List.singleton <|
                div [ class "row" ] <|
                    Maybe.values
                        [ Just <|
                            div [ class "col-md order-2" ] <|
                                List.singleton <|
                                    appView model
                        , if not model.session.settingsOpen then
                            Nothing

                          else
                            Just <|
                                div [ class "col-md-auto order-first order-md-3" ]
                                    [ settingsView model ]
                        ]
        ]


settingsView : Model -> Html Msg
settingsView model =
    let
        displayExample =
            Appl ()
                (Appl ()
                    (Term () "A")
                    (FreeVar () "x")
                )
                (Appl ()
                    (Term () "B")
                    (FreeVar () "y")
                )
    in
    div [ class "card" ]
        [ h5 [ class "card-header" ] [ text "Settings" ]
        , ul [ class "list-group list-group-flush" ]
            [ li [ class "list-group-item" ]
                [ heading "Display Style"
                , [ SymbolString
                  , BorderedTree
                  , PrettyTree
                  ]
                    |> List.map
                        (\style ->
                            div [ class "col px-0" ]
                                [ button
                                    [ class "btn"
                                    , classList
                                        [ ( "border border-success"
                                          , style == model.session.displayStyle
                                          )
                                        ]
                                    , onClick <| SetDisplayStyle style
                                    ]
                                    [ plainExprView style displayExample ]
                                ]
                        )
                    |> div [ class "row align-items-center px-3" ]
                ]
            ]
        ]


appView : Model -> Html Msg
appView model =
    div [ class "d-flex flex-column" ] <|
        case model.app of
            Editing { program, rules, searchGoal } ->
                [ heading "Rewrite rules"
                , div [ class "row" ]
                    [ div [ class "col-md" ] [ rulesInput model.session.rulesInput ]
                    , div [ class "col-md" ] [ editingRulesView model.session.displayStyle rules ]
                    ]
                , heading "Search goal"
                , div [ class "row" ]
                    [ div [ class "col-md" ]
                        [ div [ class "d-flex" ] [ goalInput model.session.searchGoalInput ] ]
                    , div [ class "col-md" ]
                        [ if model.session.searchGoalInput == "" then
                            div [] []

                          else
                            searchGoalView model.session.displayStyle searchGoal
                        ]
                    ]
                , heading "Expression to evaluate"
                , div [ class "row" ]
                    [ div [ class "col-md" ]
                        [ div [ class "d-flex flex-row" ]
                            [ programInput model.session.programInput
                            , button
                                [ class "btn btn-primary ml-1"
                                , onClick FinishEdit
                                ]
                                [ text "Go" ]
                            ]
                        ]
                    , div [ class "col-md" ] [ programView model.session.displayStyle program ]
                    ]
                ]

            Halted haltedData ->
                [ executionView model.session haltedData <|
                    div [ class "buttonRow mt-3" ] <|
                        [ button
                            [ class "btn btn-light border border-secondary"
                            , onClick StartEdit
                            ]
                            [ text "Edit" ]
                        , button
                            [ class "btn btn-primary"
                            , onClick StartRunning
                            ]
                            [ text "Run" ]
                        , button
                            [ class "btn btn-primary"
                            , onClick StepRules
                            ]
                            [ text "Step" ]
                        ]
                            ++ (case haltedData.searchGoal of
                                    Nothing ->
                                        []

                                    Just goal ->
                                        [ button
                                            [ class "btn btn-primary"
                                            , onClick (StartSearch goal)
                                            ]
                                            [ text "Search" ]
                                        ]
                               )
                ]

            Running _ runningData ->
                [ executionView model.session runningData <|
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


executionView : SessionData -> ExecutionData -> Html Msg -> Html Msg
executionView { displayStyle } { initialProgram, rules, currentState, history, searchGoal } buttons =
    div [ class "row" ] <|
        [ div [ class "col-md" ] <|
            [ heading "Rewrite rules"
            , if List.isEmpty rules then
                div [ class "font-italic" ]
                    [ text "There are no rewrite rules, which may be a bit dull." ]

              else
                executionRulesView displayStyle rules
            , heading "Initial expression"
            , div [ class "pl-3" ] [ plainExprView displayStyle initialProgram ]
            ]
                ++ (case searchGoal of
                        Nothing ->
                            []

                        Just expr ->
                            [ heading "Search Goal"
                            , div [ class "pl-3" ] [ plainExprView displayStyle expr ]
                            ]
                   )
                ++ [ heading "Current expression"
                   , div [ class "pl-3" ] [ plainExprView displayStyle currentState ]
                   , buttons
                   ]
        , div [ class "col-md" ]
            [ heading "History"
            , historyView displayStyle history
            ]
        ]


rulesInput : String -> Html Msg
rulesInput input =
    textarea
        [ Attr.rows 12
        , onInput ChangeRulesInput
        ]
        [ text input ]


goalInput : String -> Html Msg
goalInput input =
    Html.input
        [ Attr.type_ "text"
        , onInput ChangeSearchGoalInput
        , value input
        , class "flex-grow-1"
        ]
        []


programInput : String -> Html Msg
programInput input =
    Html.input
        [ Attr.type_ "text"
        , onInput ChangeProgramInput
        , value input
        , class "flex-grow-1"
        ]
        []


editingRulesView : DisplayStyle -> ParseState (List RewriteRule) -> Html Msg
editingRulesView displayStyle ps =
    case ps of
        InitialParseState ->
            div [] [ text "Enter some rules!" ]

        ShowingError err ->
            parseErrorView err

        ShowingSuccessfulParse rules ->
            rulesViewHelper displayStyle Nothing Nothing <|
                List.map (\r -> ( r, Forward )) rules


executionRulesView : DisplayStyle -> List ( RewriteRule, RuleDirection ) -> Html Msg
executionRulesView displayStyle =
    rulesViewHelper displayStyle
        (Just <|
            \ix ->
                td [] <|
                    List.singleton <|
                        div
                            [ class <|
                                "ruleSwatch"
                                    ++ String.fromInt (modBy 20 ix)
                            ]
                            []
        )
        (Just <|
            \ix ->
                td [] <|
                    List.singleton <|
                        div
                            [ class "ruleControls btn-group btn-group-sm border border-secondary rounded" ]
                            [ button
                                [ class "btn btn-light px-1"
                                , title "Step rule backward"
                                , onClick <| StepSingleRule Reverse ix
                                ]
                                [ featherIcon "skip-back" ]
                            , button
                                [ class "btn btn-light px-1"
                                , title "Reverse rule direction"
                                , onClick <| ReverseRule ix
                                ]
                                [ featherIcon "refresh-ccw" ]
                            , button
                                [ class "btn btn-light px-1"
                                , title "Step rule forward"
                                , onClick <| StepSingleRule Forward ix
                                ]
                                [ featherIcon "skip-forward" ]
                            ]
        )


rulesViewHelper :
    DisplayStyle
    -> Maybe (Int -> Html Msg)
    -> Maybe (Int -> Html Msg)
    -> List ( RewriteRule, RuleDirection )
    -> Html Msg
rulesViewHelper displayStyle rowPrefix rowSuffix rules =
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
                                        [ plainExprView displayStyle pattern ]
                                , Just <|
                                    td []
                                        [ div
                                            [ classList
                                                [ ( "arr", True )
                                                , ( "reverse", dir == Reverse )
                                                ]
                                            ]
                                            [ featherIcon "arrow-right" ]
                                        ]
                                , Just <|
                                    td [ class "pl-1" ]
                                        [ plainExprView displayStyle replacement ]
                                , rowSuffix
                                    |> Maybe.andMap (Just ix)
                                ]
                    )
                |> tbody []
            ]
        ]


programView : DisplayStyle -> ParseState PlainExpr -> Html Msg
programView displayStyle ps =
    div [] <|
        case ps of
            InitialParseState ->
                []

            ShowingSuccessfulParse expr ->
                [ plainExprView displayStyle expr ]

            ShowingError err ->
                [ parseErrorView err ]


searchGoalView : DisplayStyle -> ParseState (Either RewriteRule PlainExpr) -> Html Msg
searchGoalView displayStyle ps =
    case ps of
        InitialParseState ->
            div [] []

        ShowingSuccessfulParse p ->
            case p of
                Left { pattern, replacement } ->
                    div [ class "d-flex flex-row align-items-center" ]
                        [ div [ class "pr-1" ]
                            [ plainExprView displayStyle pattern ]
                        , div [ class "arr" ] [ featherIcon "arrow-right" ]
                        , td [ class "pl-1" ]
                            [ plainExprView displayStyle replacement ]
                        ]

                Right expr ->
                    plainExprView displayStyle expr

        ShowingError err ->
            parseErrorView err


flatTreeRenderer : (a -> List (Attribute Msg)) -> Renderer a (Html Msg)
flatTreeRenderer attrs =
    { term =
        \a t ->
            div
                (classList
                    [ ( "term", True )
                    , ( "longName", isLongIdentifier t )
                    ]
                    :: attrs a
                )
                [ text t ]
    , freeVar =
        \a v ->
            div
                (classList
                    [ ( "freeVar", True )
                    , ( "longName", isLongIdentifier v )
                    ]
                    :: attrs a
                )
                [ text v ]
    , appl =
        \p a x y ->
            div
                (classList
                    [ ( "appl", True )
                    , ( "needsParens", p )
                    ]
                    :: attrs a
                )
                [ x, y ]
    }


prettyTreeRenderer : Renderer a TreeViz.Tree
prettyTreeRenderer =
    { term =
        \_ t ->
            TreeViz.Leaf t
    , freeVar =
        \_ v ->
            TreeViz.Leaf v
    , appl =
        \_ _ x y ->
            TreeViz.Node x y
    }


isLongIdentifier : String -> Bool
isLongIdentifier str =
    String.length str
        > 1
        && (String.toList str
                |> List.filter (\c -> not <| Set.member c termSyms)
                |> List.length
                |> (\x -> x > 1)
           )


plainExprView : DisplayStyle -> Expr a -> Html Msg
plainExprView style expr =
    if style == PrettyTree then
        expr
            |> renderExpr prettyTreeRenderer
            |> TreeViz.layoutTree
            |> TreeViz.renderSvg TreeViz.defaultRenderConfig
            |> List.singleton
            |> div [ class "prettyTree" ]

    else
        expr
            |> renderExpr (flatTreeRenderer (always []))
            |> List.singleton
            |> div
                [ class "expr"
                , if style == SymbolString then
                    class "symbolString"

                  else
                    class "borderedTree"
                ]


rewrittenExprView : DisplayStyle -> RewrittenExpr -> Html Msg
rewrittenExprView style expr =
    if style == PrettyTree then
        expr
            |> renderExpr prettyTreeRenderer
            |> TreeViz.layoutTree
            |> TreeViz.renderSvg TreeViz.defaultRenderConfig
            |> List.singleton
            |> div [ class "prettyTree" ]

    else
        expr
            |> renderExpr
                (flatTreeRenderer <|
                    \{ rewrittenFrom, rewrittenTo } ->
                        Maybe.values
                            [ rewrittenFrom
                                |> Maybe.map
                                    (modBy 20
                                        >> String.fromInt
                                        >> (\s -> "rewriteFrom" ++ s)
                                        >> class
                                    )
                            , rewrittenTo
                                |> Maybe.map
                                    (modBy 20
                                        >> String.fromInt
                                        >> (\s -> "rewriteTo" ++ s)
                                        >> class
                                    )
                            ]
                )
            |> List.singleton
            |> div
                [ class "expr"
                , if style == SymbolString then
                    class "symbolString"

                  else
                    class "borderedTree"
                ]


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


historyView : DisplayStyle -> List RewrittenExpr -> Html Msg
historyView displayStyle hist =
    hist
        |> List.foldl
            (\expr rendered ->
                rewrittenExprView displayStyle expr :: rendered
            )
            []
        |> Html.ul [ class "list-unstyled historyView" ]



-- UTILS / MISC


featherIcon : String -> Html msg
featherIcon name =
    Svg.svg
        [ SvgAttr.class "feather" ]
        [ Svg.use [ SvgAttr.xlinkHref <| "feather-sprite.svg#" ++ name ] [] ]
