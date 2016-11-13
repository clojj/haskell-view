module HvCss exposing (CssClasses(..), CssIds(..), css)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)

type CssClasses = ITlineComment | ITblockComment | ITconid | ITmodule | ITtodo

type CssIds = Page

css : Stylesheet
css =
    (stylesheet << namespace "haskell-view")
        [ body
            [ overflowX auto
            , minWidth (px 1280)
            ]
        , (#) Page
            [ backgroundColor (rgb 200 128 64)
            , color (hex "CCFFFF")
            , width (pct 100)
            , height (pct 100)
            , boxSizing borderBox
            , padding (px 8)
            , margin zero
            ]
        , (.) ITlineComment [ color commentColor ]
        , (.) ITblockComment [ color commentColor ]
        , (.) ITconid [ color typeColor ]
        , (.) ITmodule [ color moduleColor ]
        -- , (.) ITtodo [ color todoColor ]
        ]

commentColor : Color
commentColor = hex "AA9999"

typeColor : Color
typeColor = hex "339999"

moduleColor : Color
moduleColor = hex "CC5555"

todoColor : Color
todoColor = hex "8A2BE"
