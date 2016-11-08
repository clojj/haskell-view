module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Http
import Task exposing (..)
import Debug
import String exposing (..)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onClick)

import HvCss
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "haskell-view"
main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { html : Html Msg }


type alias Location =
    { line : Int
    , col : Int
    }


type alias Token =
    { name : String
    , from : Location
    , to : Location
    }


initialModel : Model
initialModel =
    { html = Html.text "initializing.." }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getHaskell "TestMod" )


-- UPDATE

type Msg
    = GetSource String
    | GetSourceSucceed String
    | GetSourceFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSource name ->
            ( model, getHaskell name )

        GetSourceSucceed data ->
            -- TODO replace by folding over interleaved tokens/source
            case data of
                "" ->
                    ( Model (Html.text "error"), Cmd.none )

                src -> let html = createView src
                        in
                          ( (Model (Debug.log "HTML\n" html)), Cmd.none )

        GetSourceFail _ ->
            ( model, Cmd.none )



-- perform HTTP


getHaskell : String -> Cmd Msg
getHaskell path =
    let
        url =
            "http://localhost:8081/source/" ++ path
    in
        Task.perform GetSourceFail GetSourceSucceed <| Http.getString url



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { html } =
    html



-- create Html from update


createView : String -> Html Msg
createView src =
    Html.div []
        [ Html.div [] (List.map viewLine (split "\n" src)) ]


viewLine : String -> Html Msg
viewLine line =
    Html.div [ class [ HvCss.ITlineComment ] ] [ Html.text line, Html.br [] [] ]
