module HvCss exposing (CssClasses(..), CssIds(..), css)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)


type CssClasses = ITlineComment

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
        , (.) ITlineComment
            [ color primaryCommentColor
            ]
        ]


primaryCommentColor : Color
primaryCommentColor = hex "AA9999"
