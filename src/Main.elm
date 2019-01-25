module Main exposing (getCurrentElement, init, main, update)

import Browser
import Dict exposing (Dict)
import HOF
import Html exposing (Attribute, Html, button, div, h1, option, select, text)
import Html.Attributes exposing (..)
import Html.Events
    exposing
        ( keyCode
        , on
        , onClick
        , onInput
        )
import Json.Decode as Json
import ListType exposing (ListTypeData)
import Operation exposing (Operation)
import SeeFpType exposing (SeeFpType(..))



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


type alias Model =
    { listTypeData : Maybe ListTypeData
    , index : Int
    , hof : String
    , operation : Maybe Operation
    }


init : ( Model, Cmd Msg )
init =
    ( { listTypeData = Nothing
      , index = 0
      , hof = ""
      , operation = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Update =
    Model -> Model


type Msg
    = ChooseList String
    | ChooseOperation String
    | ChooseHOF String
    | StepLeft
    | StepRight
    | Noop


decIndex : Update
decIndex m =
    { m | index = m.index - 1 }


incIndex : Update
incIndex m =
    { m | index = m.index + 1 }


popModel : Update
popModel m =
    { m | operation = Maybe.map Operation.pop m.operation }


stepLeft : Model -> Model
stepLeft m =
    case m.operation of
        Nothing ->
            m

        Just op ->
            m |> decIndex |> popModel


getList : Model -> List SeeFpType
getList m =
    m.listTypeData
        |> Maybe.map (Tuple.second >> .list)
        |> Maybe.withDefault []


pushModel : Update
pushModel m =
    let
        hofs =
            Dict.fromList
                [ ( HOF.map, Operation.map )
                , ( HOF.filter, Operation.filter )
                , ( HOF.reduce, Operation.reduce )
                ]
    in
    m
        |> getList
        |> elementAt m.index
        |> Maybe.map
            (\x ->
                case ( m.operation, m.hof ) of
                    ( Nothing, _ ) ->
                        m

                    ( _, "" ) ->
                        m

                    ( Just op, h ) ->
                        let
                            hofHandler =
                                Maybe.withDefault (\a b -> b) <| Dict.get h hofs
                        in
                        { m | operation = Just <| hofHandler x op }
            )
        |> Maybe.withDefault m


stepRight : Update
stepRight m =
    case m.operation of
        Nothing ->
            m

        Just op ->
            m |> pushModel |> incIndex


resetIndex : Update
resetIndex m =
    { m | index = 0 }


resetOperation : Update
resetOperation m =
    { m | operation = Nothing }


setList : String -> Update
setList listType m =
    { m | listTypeData = ListType.fromString listType }


setHOF : String -> Update
setHOF hof m =
    { m | hof = hof }


setOperation : String -> Update
setOperation op m =
    { m
        | operation =
            Operation.fromString op
                (m.listTypeData
                    |> Maybe.map (Tuple.second >> .hofs >> Dict.fromList)
                    |> Maybe.andThen (Dict.get m.hof)
                    |> Maybe.withDefault []
                )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChooseList l ->
            ( model
                |> resetIndex
                |> resetOperation
                |> setHOF ""
                |> setList l
            , Cmd.none
            )

        ChooseHOF h ->
            ( model |> resetIndex |> resetOperation |> setHOF h, Cmd.none )

        ChooseOperation o ->
            ( model |> setOperation o |> resetIndex
            , Cmd.none
            )

        StepLeft ->
            if model.index - 1 < 0 || model.operation == Nothing then
                ( model, Cmd.none )

            else
                ( stepLeft model, Cmd.none )

        StepRight ->
            if (model.index + 1 > (model |> getList |> List.length)) || model.operation == Nothing then
                ( model, Cmd.none )

            else
                ( stepRight model, Cmd.none )


elementAt : Int -> List a -> Maybe a
elementAt index =
    List.drop index >> List.head


handleKeydown x =
    case x of
        37 ->
            StepLeft

        39 ->
            StepRight

        _ ->
            Noop



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "SeeFP", onKeydown handleKeydown ]
        [ h1 [ class (bem "header") ] [ text "See FP" ]
        , div [ class (bem "stage") ]
            [ div [ class (bem "operation") ]
                [ text
                    (case model.operation of
                        Nothing ->
                            "n/a"

                        Just o ->
                            o.name
                    )
                ]
            , div [ class (bem "signature") ]
                [ text
                    (case model.operation of
                        Nothing ->
                            "n/a"

                        Just o ->
                            o.signature
                    )
                ]
            , div [ class (bem "selectedList") ]
                (model
                    |> showList
                    |> List.indexedMap Tuple.pair
                    |> List.map
                        (\( i, x ) ->
                            div
                                [ class
                                    (bem "listElement "
                                        ++ (case
                                                ( i == model.index
                                                , model.listTypeData /= Nothing
                                                )
                                            of
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
            , div [ class (bem "transformedList") ]
                (model
                    |> showTranformedList
                    |> List.map
                        (\x ->
                            div
                                [ class
                                    (bem "listElement "
                                        ++ (case model.listTypeData /= Nothing of
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
            , div [ class (bem "currentElement") ]
                [ text <| getCurrentElement model ]
            ]
        , select [ onInput ChooseList ]
            [ option
                [ disabled True, selected True ]
                [ text "Pick a list" ]
            , makeOption "nums" "Nums" (model.listTypeData |> Maybe.map Tuple.first |> Maybe.withDefault "")
            , makeOption "names" "Names" (model.listTypeData |> Maybe.map Tuple.first |> Maybe.withDefault "")
            , makeOption "cats" "Cats" (model.listTypeData |> Maybe.map Tuple.first |> Maybe.withDefault "")
            ]
        , select [ onInput ChooseHOF, disabled (model.listTypeData == Nothing) ]
            [ option
                [ disabled True, selected (model.hof == "") ]
                [ text "Pick a higher order function" ]
            , makeOption "map" "map" model.hof
            , makeOption "filter" "filter" model.hof
            , makeOption "reduce" "reduce" model.hof
            ]
        , select
            [ onInput ChooseOperation
            , disabled
                (List.isEmpty <|
                    operationOptions model
                )
            ]
            ([ option
                [ disabled True, selected (model.operation == Nothing) ]
                [ text "Pick an operation" ]
             ]
                ++ (model
                        |> operationOptions
                        |> List.map
                            (\x ->
                                makeOption x
                                    x
                                    (model.operation |> Maybe.map .name |> Maybe.withDefault "")
                            )
                   )
            )
        , div [ class <| bem <| "steps" ]
            [ button [ class (bem "stepLeft"), onClick StepLeft ] [ text "⏪" ]
            , button [ class (bem "stepRight"), onClick StepRight ] [ text "⏩" ]
            ]
        ]


operationOptions : Model -> List String
operationOptions m =
    m.listTypeData
        |> Maybe.map (Tuple.second >> .hofs >> Dict.fromList)
        |> Maybe.andThen (Dict.get m.hof)
        |> Maybe.map (List.map Tuple.first)
        |> Maybe.withDefault []


bem : String -> String
bem element =
    "SeeFP__" ++ element


makeOption : String -> String -> String -> Html Msg
makeOption v t current =
    option
        [ value v, selected (current == v) ]
        [ text t ]


showList : Model -> List String
showList model =
    model
        |> getList
        |> List.map SeeFpType.toString


showTranformedList : Model -> List String
showTranformedList model =
    case model.operation of
        Just o ->
            o.repository
                |> List.map SeeFpType.toString

        Nothing ->
            [ "_" ]


getCurrentElement : Model -> String
getCurrentElement model =
    model
        |> getList
        |> List.drop model.index
        |> List.head
        |> Maybe.map SeeFpType.toString
        |> Maybe.withDefault "n/a"


onKeydown : (Int -> msg) -> Attribute msg
onKeydown message =
    on "keydown" (Json.map message keyCode)
