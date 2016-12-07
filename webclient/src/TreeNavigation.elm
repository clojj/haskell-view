module TreeNavigation exposing (..)

import String exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug
import List exposing (..)
import Chae.Id as Id exposing (Id)
import Chae.Node as Node exposing (Node)
import Chae.Tree as Tree exposing (Tree)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Item =
    { id : String
    , name : String
    , parentIds : List String
    }


type alias Model =
    { tree : Tree Item
    , opened : List Id
    }


items : List Item
items =
    [ { id = "a", name = "root a", parentIds = [] }
    , { id = "a10", name = "a10", parentIds = [ "a" ] }
    , { id = "a11", name = "a11", parentIds = [ "a" ] }
    , { id = "b", name = "root with nested", parentIds = [] }
    , { id = "c", name = "nest", parentIds = [ "b" ] }
    , { id = "d", name = "more nested", parentIds = [ "c" ] }
    ]

initialTree : Tree Item
initialTree = Tree.fromList (.id) (.parentIds) items


init : ( Model, Cmd Msg )
init =
    ( { tree = initialTree
      , opened = List.map .id items
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Search String
    | Toggle Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- chae-tree
        NoOp ->
            model ! []

        Search s ->
            let
                tree_ = 
                  case s of
                    "" -> initialTree
                    _ -> Tree.filter (\item -> contains s item.name) model.tree
            in
                { model | tree = tree_ } ! []

        Toggle id ->
            case partition (\o -> o == id) model.opened of
                ( [], rest ) ->
                    { model | opened = id :: rest } ! []

                ( _, rest ) ->
                    { model | opened = rest } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    viewTree model


isOpened : List Id -> Node a -> Bool
isOpened list node =
    member (Node.id node) list


itemView : Model -> Node Item -> Html Msg
itemView model node =
    let
        item =
            Node.root node

        open =
            isOpened (.opened model) node

        symbol =
            if List.length (Node.children node) > 0 then
                if open then
                    "[-] "
                else
                    "[+] "
            else
                "[ ] "
    in
        li []
            [ a [ onClick (Toggle (Node.id node)) ]
                [ text (symbol ++ item.name) ]
            , if open then
                listView model (Node.children node)
              else
                text ""
            ]


listView : Model -> Tree Item -> Html Msg
listView model items =
    ul []
        (List.map (\n -> itemView model n) items)


viewTree : Model -> Html Msg
viewTree model =
    div []
        [ input [ placeholder "Search", onInput Search ] []
        , listView model model.tree
        ]
