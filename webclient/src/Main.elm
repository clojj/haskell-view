module Main exposing (..)

import Html exposing ( Html )
import Html.App as Html
import Http
import Task exposing (..)
import Debug
import String exposing (..)

-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onClick)

import HvCss
import Html.CssHelpers


{ id, class, classList } = Html.CssHelpers.withNamespace "haskell-view"

main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Location =
  { line : Int
  , col : Int
  }

type alias Token =
  { name : String
  , from : Location
  , to : Location
  }

type alias Model =
    { src : String
    , html : Html Msg
    , tokens : String
    }


initialModel : Model
initialModel =
  { src = ""
  , html = Html.text ""
  , tokens = ""
  }


init : ( Model, Cmd Msg )
init = ( initialModel, getHaskell "Lib" )


-- UPDATE

type Msg
  = GetHaskell
  | FetchSucceed String
  | FetchFail Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        GetHaskell ->
          (model, getHaskell "TODO")

        FetchSucceed data ->
           case (split "<EOF>" data) of
            [] -> (Model "err" (Html.text "err") "", Cmd.none)
            src :: tokens ->
              let t = case (List.head tokens) of
                        Just ts -> ts
                        Nothing -> "no tokens"
                  html = createView src t
                  in ((Model model.src (Debug.log "HTML\n" html) model.tokens), Cmd.none)

        FetchFail _ ->
          (model, Cmd.none)

-- HTTP

getHaskell : String -> Cmd Msg
getHaskell path =
  let url = "http://localhost:8081/source/" ++ path
  in
    Task.perform FetchFail FetchSucceed <| Http.getString url



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view { src, html, tokens } = html

createView : String -> String -> Html Msg
createView src tokens =
    Html.div [] [
      Html.div [] (List.map viewLine (split "\n" src)),
      Html.div [] [Html.text ("TOKENS: " ++ tokens)]
    ]

viewLine : String -> Html Msg
viewLine line =
    Html.div [ class [HvCss.ITlineComment]] [ Html.text line, Html.br [] []]

