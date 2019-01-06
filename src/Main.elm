module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


nums : List Int
nums =
    [ 4, 8, 15, 16, 23, 42 ]


names : List String
names =
    [ "Harry", "Hermione", "Ron" ]


type ListType
    = Nums
    | Names


type alias Model =
    { listType : Maybe ListType
    , nums : List Int
    , names : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing nums names, Cmd.none )



---- UPDATE ----


type Msg
    = ChooseList String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseList l ->
            ( { model | listType = listTypeFromString l }, Cmd.none )


listTypeFromString : String -> Maybe ListType
listTypeFromString list =
    case list of
        "Nums" ->
            Just Nums

        "Names" ->
            Just Names

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "SeeFP" ]
        [ h1 [ class "SeeFP__Header" ] [ text "SEE FP" ]
        , div [ class "SeeFP__SelectedList" ]
            [ text <| showList model ]
        , select [ onInput ChooseList ]
            [ option
                [ disabled True, selected True ]
                [ text "Pick a list" ]
            , makeOption "Nums" "Nums"
            , makeOption "Names" "Names"
            ]
        ]


makeOption : String -> String -> Html Msg
makeOption v t =
    option
        [ value v ]
        [ text t ]


showList : Model -> String
showList model =
    case model.listType of
        Just Nums ->
            model.nums
                |> List.map String.fromInt
                |> String.join " "

        Just Names ->
            model.names
                |> String.join " "

        Nothing ->
            "Select a list"
