module Main exposing (..)

import Char
import Parser exposing (Parser, (|=), (|.))
import Result
import Set

type Expr = Term String
          | Appl Expr Expr

type Tokens = Symbol String
            | Parens (List Tokens)

lexer : Parser Tokens
lexer =
    Parser.oneOf
        [ Parser.succeed Symbol
              |= Parser.variable
                  { start = Char.isAlpha
                  , inner = always False
                  , reserved = Set.empty
                  }
        , Parser.succeed Symbol
            |. Parser.symbol "["
            |= Parser.variable
                   { start = \c -> c /= ']'
                   , inner = \c -> c /= ']'
                   , reserved = Set.empty
                   }
            |. Parser.symbol "]"
        , Parser.succeed Parens
            |= Parser.sequence
                  { start = "("
                  , separator = ""
                  , end = ")"
                  , spaces = Parser.spaces
                  , item = Parser.lazy <| \_ -> lexer
                  , trailing = Parser.Optional
                  }
        ]
    |. Parser.spaces

buildAst : Tokens -> Result String Expr
buildAst tok =
    case tok of
        Symbol sym -> Ok <| Term sym
        Parens [] -> Err "Empty parenthetical group"
        Parens [nested] -> buildAst nested
        Parens [x, y] -> Result.map2 Appl (buildAst x) (buildAst y)
        Parens (x :: y :: zs) -> Result.map2 Appl
                                   (buildAst x)
                                   (buildAst <| Parens <| y :: zs)

parseExpr : String -> Result String Expr
parseExpr str =
    (Parser.run lexer <| "(" ++ str ++ ")")
        |> Result.mapError Parser.deadEndsToString
        |> Result.andThen buildAst

