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
    { tokens : String
    , src : String
    }


initialModel : Model
initialModel =
  { tokens = ""
  , src = ""
  }


init : ( Model, Cmd Msg )
init = ( initialModel, getHaskell "Lib" )


-- UPDATE

type Msg
  = NoOp
  | GetHaskell
  | ProcessHaskell
  | FetchSucceed String
  | FetchFail Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetHaskell ->
          (model, getHaskell "TODO")

        FetchSucceed data ->
           let
            newModel = (Model model.tokens (Debug.log "response: " data))
           in
            -- TODO elm-update-extra or elm-return or Task.perform ?
            update ProcessHaskell newModel

        FetchFail _ ->
          (model, Cmd.none)

        ProcessHaskell ->
          (Model model.tokens (Debug.log "process: " model.src), Cmd.none)

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
view { tokens, src } =
    Html.div [] (List.map viewLine (split "\n" src))

viewLine : String -> Html Msg
viewLine line =
    Html.div [ class [HvCss.ITlineComment]] [ Html.text line, Html.br [] []]

