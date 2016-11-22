module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Debug
import String exposing (..)
import List.Split exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { modules : List String,
      html    : Html Msg }


init : ( Model, Cmd Msg )
init =
    -- ( { modules = [], html = Html.text "" }, getHaskell "TestMod" LoadSourceSucceed )
    ( { modules = [], html = Html.text "" }, getModules LoadModulesSucceed )



-- UPDATE


type Msg
    = LoadNewSource String
    | LoadSourceSucceed (Result Http.Error String)
    | LoadModulesSucceed (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNewSource name ->
            ( model, getHaskell name LoadSourceSucceed )

        LoadModulesSucceed res ->
            case res of
              Result.Ok data ->
                case data of 
                  "" -> ( {model | html = Html.text "error"}, Cmd.none )
          
                  src -> let srcModules = split "," data
                             mod = case List.head srcModules of
                                     Just m -> m
                                     _      -> ""
                         in ( {model | modules = srcModules,
                                       html = viewModules srcModules },
                              Cmd.none )
              Result.Err err ->
                  let _ = Debug.log "Error " err
                    in (model, Cmd.none)
        
        LoadSourceSucceed res ->
            case res of
              Result.Ok data ->
                case data of
                    "" -> ( {model | html = Html.text "error"}, Cmd.none )
            
                    src -> let srcHtml = createView src
                           in ( {model | html = srcHtml}, Cmd.none )
              Result.Err err ->
                  let _ = Debug.log "Error " err
                    in (model, Cmd.none)
        

-- perform HTTP

getHaskell : String -> (Result Http.Error String -> msg) -> Cmd msg
getHaskell path msg =
  Http.getString ("http://localhost:8081/source/" ++ path) |> Http.send msg

getModules : (Result Http.Error String -> msg) -> Cmd msg
getModules msg =
  Http.getString ("http://localhost:8081/source/.modules") |> Http.send msg


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view { html } = html

-- create Html from update

-- TODO use http://package.elm-lang.org/packages/evancz/elm-sortable-table/latest
viewModules : List String -> Html Msg
viewModules modules =
  case modules of
    [] -> Html.text "no modules!"
    _ -> Html.div [class "haskell-viewITmodule"] (List.map createModuleLink modules)

createModuleLink : String -> Html Msg    
createModuleLink mod =    
  Html.div [onClick (LoadNewSource mod)] [Html.text mod]
  
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
        [ Html.div [] [ Html.table [] (List.map viewLine (split "\n" src)) ]]

viewLine : String -> Html Msg
viewLine line =
    let tokens = chunksOfLeft 2 (split "\x001F" line)
    in Html.tr [] [ Html.td [] (List.map viewToken tokens) ]

-- TODO classes: generate into hv.css
-- see TokenGenerator.hs
viewToken : List String -> Html Msg
viewToken token = case token of
    tname :: txt :: _ -> Html.span [ lineStyle, class ("haskell-view" ++ tname) ]
      [ Html.text txt ] -- (String.map (\ch -> if (ch == ' ') then '_' else ch) txt)

    _ :: [] -> Html.span [lineStyle] [Html.text " "]
    [] -> Html.span [lineStyle] [Html.text " "]

lineStyle : Attribute a
lineStyle = Html.Attributes.style [("display", "inline-block"), ("white-space", "pre")]
