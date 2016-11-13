module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Http
import Task exposing (..)
import Debug
import String exposing (..)
import List.Split exposing (..)


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

                src ->
                    let
                        html =
                            createView src
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
        -- TODO parse src
        -- <tr>
        --   <td id="L23" class="blob-num js-line-number" data-line-number="23"></td>
        --   <td id="LC23" class="blob-code blob-code-inner js-file-line">  <span class="pl-k">if</span> l1 - l == <span class="pl-c1">0</span> &amp;&amp; c1 == c <span class="pl-k">then</span></td>
        -- </tr>
        -- <tr>
        --   <td id="L24" class="blob-num js-line-number" data-line-number="24"></td>
        --   <td id="LC24" class="blob-code blob-code-inner js-file-line">    <span class="pl-c">-- virtual tokens (semi, vocurly etc)</span></td>
        -- </tr>
        [ Html.div [] [ Html.table [] (List.map (\l -> cleanLine l |> viewLine) (split "\n" src)) ]]

cleanLine : String -> String
cleanLine line = 
  let s1 = case startsWith "{}" line of
             True -> dropLeft 2 line
             _ -> line
      s2 = case endsWith "{}" s1 of
             True -> dropRight 2 s1
             _ -> s1
  in s2
  
viewLine : String -> Html Msg
viewLine line =
    -- Html.div [ class [ HvCss.ITconid ] ] [ Html.text line, Html.br [] [] ]
    -- Html.tr [] [ Html.td [] (List.map viewToken (split "{}" line)) ]
    let tokens = chunksOfLeft 2 (split "{}" line)
    in Html.tr [] [ Html.td [] (List.map viewToken' tokens) ]

viewToken : String -> Html Msg
viewToken token =
    Html.span [ class [ HvCss.ITconid ] ] [ Html.text token ]

viewToken' : List String -> Html Msg
viewToken' token = case token of

    tname :: txt :: _ -> Html.span [ class [ mapTokenToCss tname ] ] [ Html.text (String.map (\ch -> if (ch == ' ') then '_' else ch) txt) ]

    _ :: [] -> Html.span [] []
    [] -> Html.span [] []

mapTokenToCss : String -> HvCss.CssClasses
mapTokenToCss tname =
  case tname of
    "ITmodule" -> HvCss.ITmodule
    "ITconid" -> HvCss.ITconid
    "ITblockComment" -> HvCss.ITblockComment
    "ITlineComment" -> HvCss.ITlineComment
    "ITtodo" -> HvCss.ITtodo
    otherwise -> HvCss.ITtodo