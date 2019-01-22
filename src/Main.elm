module Main exposing (getCurrentElement, init, main, update)

import Browser
import HOF exposing (..)
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
import ListType
    exposing
        ( ListType(..)
        , operationsFor
        )
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
    { listType : Maybe ListType
    , nums : List SeeFpType
    , names : List SeeFpType
    , cats : List SeeFpType
    , index : Int
    , hof : Maybe HOF
    , operation : Maybe Operation
    }


init : ( Model, Cmd Msg )
init =
    ( { listType = Nothing
      , nums = ListType.nums
      , names = ListType.names
      , cats = ListType.cats
      , index = 0
      , hof = Nothing
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
    case m.operation of
        Nothing ->
            m

        Just op ->
            { m | operation = Just <| Operation.pop op }


stepLeft : Model -> Model
stepLeft m =
    case m.operation of
        Nothing ->
            m

        Just op ->
            m |> decIndex |> popModel


getList : Model -> List SeeFpType
getList m =
    case m.listType of
        Just Nums ->
            m.nums

        Just Names ->
            m.names

        Just Cats ->
            m.cats

        _ ->
            []


pushModel : Update
pushModel m =
    case elementAt m.index (getList m) of
        Just x ->
            case ( m.operation, m.hof ) of
                ( Nothing, _ ) ->
                    m

                ( _, Nothing ) ->
                    m

                ( Just op, Just Map ) ->
                    { m | operation = Just <| Operation.push x op }

                ( Just op, Just Filter ) ->
                    { m | operation = Just <| Operation.filter x op }

                _ ->
                    m

        _ ->
            m


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
    { m | listType = ListType.fromString listType }


setHOF : String -> Update
setHOF hof m =
    { m | hof = HOF.fromString hof }


setOperation : String -> Update
setOperation op m =
    { m | operation = Operation.fromString op }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChooseList l ->
            ( model |> resetIndex |> resetOperation |> setList l
            , Cmd.none
            )

        ChooseHOF h ->
            ( model |> resetIndex |> resetOperation |> setHOF h
            , Cmd.none
            )

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
            if model.index + 1 > List.length model.nums || model.operation == Nothing then
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
            , div [ class (bem "transformedList") ]
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
            , div [ class (bem "currentElement") ]
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
        , select [ onInput ChooseHOF ]
            [ option
                [ disabled True, selected True ]
                [ text "Pick a higher order function" ]
            , makeOption "Map" "map"
            , makeOption "Filter" "filter"
            , makeOption "Reduce" "reduce"
            ]
        , select [ onInput ChooseOperation ]
            ([ option
                [ disabled True, selected True ]
                [ text "Pick an operation" ]
             ]
                ++ (model.listType
                        |> Maybe.map operationsFor
                        |> Maybe.andThen (hofOperations model.hof)
                        |> Maybe.withDefault []
                        |> List.map (\x -> makeOption x x)
                   )
            )
        , div [ class <| bem <| "steps" ]
            [ button [ class (bem "stepLeft"), onClick StepLeft ] [ text "⏪" ]
            , button [ class (bem "stepRight"), onClick StepRight ] [ text "⏩" ]
            ]
        ]


hofOperations :
    Maybe HOF
    ->
        { map : List String
        , filter : List String
        , reduce :
            List String
        }
    -> Maybe (List String)
hofOperations hof ops =
    case hof of
        Just Map ->
            Just ops.map

        Just Filter ->
            Just ops.filter

        Just Reduce ->
            Just ops.reduce

        _ ->
            Nothing


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
            model.nums |> List.map SeeFpType.toString

        Just Names ->
            model.names |> List.map SeeFpType.toString

        Just Cats ->
            model.cats |> List.map SeeFpType.toString

        Nothing ->
            [ "n/a" ]


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
    case model.listType of
        Just Nums ->
            model.nums
                |> List.drop model.index
                |> List.head
                |> Maybe.map SeeFpType.toString
                |> Maybe.withDefault "n/a"

        _ ->
            "n/a"


onKeydown : (Int -> msg) -> Attribute msg
onKeydown message =
    on "keydown" (Json.map message keyCode)
