module TreeVizTest exposing (suite)

import Char
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List
import Maybe
import Maybe.Extra as Maybe
import Result
import Result.Extra as Result
import Set
import String
import Test exposing (..)
import TreeViz exposing (..)
import Tuple


suite : Test
suite =
    describe "Tree layout"
        -- TODO: These tests are really bad. Maybe add some property tests of the aesthetic criteria?
        [ assertTreeLayout "tree 1"
            (Node (Leaf "A") (Leaf "B"))
            { nodes =
                [ { str = Nothing, x = 0.0, y = 0.0 }
                , { str = Just "A", x = -0.5, y = 1.0 }
                , { str = Just "B", x = 0.5, y = 1.0 }
                ]
            , edges =
                [ { start = { x = 0.0, y = 0.0 }
                  , end = { x = -0.5, y = 1.0 }
                  }
                , { start = { x = 0.0, y = 0.0 }
                  , end = { x = 0.5, y = 1.0 }
                  }
                ]
            }
        , assertTreeLayout "tree 2"
            (Node
                (Node
                    (Leaf "A")
                    (Node
                        (Leaf "B")
                        (Leaf "C")
                    )
                )
                (Node
                    (Node
                        (Node
                            (Leaf "D")
                            (Leaf "E")
                        )
                        (Leaf "F")
                    )
                    (Leaf "G")
                )
            )
            { nodes =
                [ { str = Nothing, x = 0, y = 0 }
                , { str = Nothing, x = -1.5, y = 1 }
                , { str = Nothing, x = 1.5, y = 1 }
                , { str = Just "A", x = -2, y = 2 }
                , { str = Nothing, x = -1, y = 2 }
                , { str = Nothing, x = 1, y = 2 }
                , { str = Just "G", x = 2, y = 2 }
                , { str = Just "B", x = -1.5, y = 3 }
                , { str = Just "C", x = -0.5, y = 3 }
                , { str = Nothing, x = 0.5, y = 3 }
                , { str = Just "F", x = 1.5, y = 3 }
                , { str = Just "D", x = 0, y = 4 }
                , { str = Just "E", x = 1, y = 4 }
                ]
            , edges =
                [ { end = { x = -2, y = 2 }, start = { x = -1.5, y = 1 } }
                , { end = { x = -1, y = 2 }, start = { x = -1.5, y = 1 } }
                , { end = { x = -1.5, y = 3 }, start = { x = -1, y = 2 } }
                , { end = { x = -0.5, y = 3 }, start = { x = -1, y = 2 } }
                , { end = { x = -1.5, y = 1 }, start = { x = 0, y = 0 } }
                , { end = { x = 1.5, y = 1 }, start = { x = 0, y = 0 } }
                , { end = { x = 0, y = 4 }, start = { x = 0.5, y = 3 } }
                , { end = { x = 1, y = 4 }, start = { x = 0.5, y = 3 } }
                , { end = { x = 0.5, y = 3 }, start = { x = 1, y = 2 } }
                , { end = { x = 1.5, y = 3 }, start = { x = 1, y = 2 } }
                , { end = { x = 1, y = 2 }, start = { x = 1.5, y = 1 } }
                , { end = { x = 2, y = 2 }, start = { x = 1.5, y = 1 } }
                ]
            }
        ]


assertTreeLayout : String -> Tree -> TreeLayout -> Test
assertTreeLayout name tree expected =
    test name <|
        \_ ->
            layoutTree tree
                |> Expect.all
                    [ .nodes
                        >> List.sortBy (\{ x, y } -> ( y, x ))
                        >> Expect.equalLists
                            (expected.nodes
                                |> List.sortBy (\{ x, y } -> ( y, x ))
                            )
                    , .edges
                        >> List.sortBy
                            (\{ start, end } ->
                                ( ( start.x, start.y )
                                , ( end.x, end.y )
                                )
                            )
                        >> Expect.equalLists
                            (expected.edges
                                |> List.sortBy
                                    (\{ start, end } ->
                                        ( ( start.x, start.y ), ( end.x, end.y ) )
                                    )
                            )
                    ]
