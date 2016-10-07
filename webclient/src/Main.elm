module Main exposing (..)

import Html exposing (..)
import Html.App as Html

import String exposing (..)

import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


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

type alias HsType =
  { name : String
  , from : Location
  , to : Location
  }

type alias Model =
    { json : List HsType
    , doc : String
    }


initialModel : Model
initialModel =
  { json =
    [
      { name = "a"
      , from = {line = 0, col = 2}
      , to = {line = 0, col = 3}
      }
    ]
  , doc = "  a <- x\nsecond line"
  }


init : ( Model, Cmd Msg )
init = ( initialModel, Cmd.none )

-- UPDATE

type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view { json, doc } =
    div [] (List.map viewLine (split "\n" doc))

viewLine : String -> Html Msg
viewLine line =
    div [] [ text line, br [] []]

showName : HsType -> String
showName hsType = "name: " ++ hsType.name
