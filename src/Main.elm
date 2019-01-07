module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, h1, h3, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json



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


cats : List String
cats =
    [ "😺"
    , "😸"
    , "😹"
    , "😻"
    ]


people : List String
people =
    [ "😀"
    , "😄"
    , "😂"
    , "😍"
    ]


type HOF
    = Map
    | Filter
    | Reduce


type Operation
    = Increment


operationSignature : Operation -> String
operationSignature o =
    case o of
        Increment ->
            "x => x + 1"


type ListType
    = Nums
    | Names
    | Cats


type alias Model =
    { listType : Maybe ListType
    , nums : List Int
    , mappedNums : List Int
    , names : List String
    , cats : List String
    , index : Int
    , hof : Maybe HOF
    , operation : Maybe Operation
    }


init : ( Model, Cmd Msg )
init =
    ( { listType = Nothing
      , nums = nums
      , mappedNums = []
      , names = names
      , cats = cats
      , index = 0
      , hof = Nothing
      , operation = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChooseList String
    | ChooseOperation String
    | StepLeft
    | StepRight
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChooseList l ->
            ( { model | listType = listTypeFromString l }, Cmd.none )

        ChooseOperation o ->
            ( { model | operation = operationFromString o }, Cmd.none )

        StepLeft ->
            if model.index - 1 < 0 || model.operation == Nothing then
                ( model, Cmd.none )

            else
                ( { model
                    | index = model.index - 1
                    , mappedNums =
                        model.mappedNums
                            |> List.reverse
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.reverse
                  }
                , Cmd.none
                )

        StepRight ->
            if model.index + 1 > List.length model.nums || model.operation == Nothing then
                ( model, Cmd.none )

            else
                ( { model
                    | index = model.index + 1
                    , mappedNums =
                        case model.nums |> List.drop model.index |> List.head of
                            Just x ->
                                model.mappedNums ++ [ transformed x model ]

                            Nothing ->
                                model.mappedNums
                  }
                , Cmd.none
                )


transformed : a -> Model -> Int
transformed x model =
    case model.operation of
        Just Increment ->
            model.nums
                |> List.drop model.index
                |> List.head
                |> Maybe.withDefault 0
                |> (+) 1

        _ ->
            0


listTypeFromString : String -> Maybe ListType
listTypeFromString list =
    case list of
        "Nums" ->
            Just Nums

        "Names" ->
            Just Names

        "Cats" ->
            Just Cats

        _ ->
            Nothing


operationFromString : String -> Maybe Operation
operationFromString operation =
    case operation of
        "increment" ->
            Just Increment

        _ ->
            Nothing


startOfList : Model -> Bool
startOfList model =
    model.index == 0


endOfList : Model -> Bool
endOfList model =
    case model.listType of
        Just Nums ->
            model.index >= List.length model.nums

        Just Names ->
            model.index >= List.length model.names

        Just Cats ->
            model.index >= List.length model.cats

        Nothing ->
            True


handleKeydown x =
    case x of
        37 ->
            StepLeft

        39 ->
            StepRight

        _ ->
            Noop


operationString : Operation -> String
operationString o =
    case o of
        Increment ->
            "Increment"


operationsFor : Maybe ListType -> List String
operationsFor lt =
    case lt of
        Just Nums ->
            [ "increment" ]

        _ ->
            []



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "SeeFP", onKeydown handleKeydown ]
        [ h1 [ class "SeeFP__header" ] [ text "See FP" ]
        , h3 [ class "SeeFP__index" ]
            [ model.index
                |> String.fromInt
                |> text
            ]
        , div [ class "SeeFP__stage" ]
            [ div [ class "SeeFP__operation" ]
                [ text
                    (case model.operation of
                        Just o ->
                            operationString o

                        Nothing ->
                            "n/a"
                    )
                ]
            , div [ class "SeeFP__signature" ]
                [ text
                    (case model.operation of
                        Just o ->
                            operationSignature o

                        Nothing ->
                            "n/a"
                    )
                ]
            , div [ class "SeeFP__selectedList" ]
                (model
                    |> showList
                    |> List.indexedMap Tuple.pair
                    |> List.map
                        (\( i, x ) ->
                            div
                                [ class
                                    (bem "listElement "
                                        ++ (case ( i == model.index, model.listType /= Nothing ) of
                                                ( True, True ) ->
                                                    "--current"

                                                ( _, False ) ->
                                                    "--na"

                                                _ ->
                                                    ""
                                           )
                                    )
                                ]
                                [ text x ]
                        )
                )
            , div [ class "SeeFP__transformedList" ]
                (model
                    |> showTranformedList
                    |> List.map
                        (\x ->
                            div
                                [ class
                                    (bem "listElement "
                                        ++ (case model.listType /= Nothing of
                                                False ->
                                                    "--na"

                                                _ ->
                                                    ""
                                           )
                                    )
                                ]
                                [ text x ]
                        )
                )
            , div [ class "SeeFP__currentElement" ]
                [ text <| getCurrentElement model ]
            ]
        , select [ onInput ChooseList ]
            [ option
                [ disabled True, selected True ]
                [ text "Pick a list" ]
            , makeOption "Nums" "Nums"
            , makeOption "Names" "Names"
            , makeOption "Cats" "Cats"
            ]
        , select [ onInput ChooseOperation ]
            ([ option
                [ disabled True, selected True ]
                [ text "Pick an operation" ]
             ]
                ++ (model.listType
                        |> operationsFor
                        |> List.map (\x -> makeOption x x)
                   )
            )
        , div [ class <| bem <| "steps" ]
            [ button [ class "SeeFP__stepLeft", onClick StepLeft ] [ text "⏪" ]
            , button [ class "SeeFP__stepRight", onClick StepRight ] [ text "⏩" ]
            ]
        ]


bem : String -> String
bem element =
    "SeeFP__" ++ element


makeOption : String -> String -> Html Msg
makeOption v t =
    option
        [ value v ]
        [ text t ]


showList : Model -> List String
showList model =
    case model.listType of
        Just Nums ->
            model.nums |> List.map String.fromInt

        Just Names ->
            model.names

        Just Cats ->
            model.cats

        Nothing ->
            [ "n/a" ]


showTranformedList : Model -> List String
showTranformedList model =
    case model.listType of
        Just Nums ->
            case model.mappedNums of
                [] ->
                    [ "_" ]

                xs ->
                    xs |> List.map String.fromInt

        Just Names ->
            model.names

        Just Cats ->
            model.cats

        Nothing ->
            [ "n/a" ]


getCurrentElement : Model -> String
getCurrentElement model =
    case model.listType of
        Just Nums ->
            model.nums
                |> List.drop model.index
                |> List.head
                |> Maybe.map
                    String.fromInt
                |> Maybe.withDefault "n/a"

        _ ->
            "n/a"


onKeydown : (Int -> msg) -> Attribute msg
onKeydown message =
    on "keydown" (Json.map message keyCode)
