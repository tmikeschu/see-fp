module Operation exposing
    ( Operation
    , filter
    , fromString
    , map
    , pop
    , reduce
    )

import Dict exposing (Dict)
import SeeFpType exposing (SeeFpFunction(..), SeeFpType(..), functionFor)


type alias Operation =
    { name : String
    , repository : List SeeFpType
    , signature : String
    , fn : SeeFpFunction
    }


type alias Update =
    Operation -> Operation


fromString : String -> List ( String, String ) -> Maybe Operation
fromString o operations =
    operations
        |> List.filter (Tuple.first >> (==) o)
        |> List.head
        |> Maybe.map
            (\( name, signature ) ->
                Operation name [] signature (functionFor name)
            )


pushRepo : a -> List a -> List a
pushRepo el rep =
    rep ++ [ el ]


popRepo : List a -> List a
popRepo =
    List.reverse
        >> List.tail
        >> Maybe.withDefault []
        >> List.reverse


pop : Update
pop op =
    { op | repository = popRepo op.repository }


map : SeeFpType -> Update
map x op =
    let
        val =
            case op.fn of
                Unary fn ->
                    fn x

                _ ->
                    x
    in
    { op | repository = pushRepo val op.repository }


filter : SeeFpType -> Update
filter x op =
    let
        val =
            case op.fn of
                Unary fn ->
                    fn x

                _ ->
                    BoolVal <| False
    in
    case val of
        BoolVal b ->
            { op
                | repository =
                    if b then
                        pushRepo x op.repository

                    else
                        op.repository
            }

        _ ->
            op


reduce : SeeFpType -> Update
reduce x op =
    let
        acc =
            op.repository
                |> List.reverse
                |> List.head
                |> Maybe.withDefault (StrVal "INIT")

        val =
            case op.fn of
                Binary fn ->
                    fn acc x

                _ ->
                    x
    in
    { op | repository = pushRepo val op.repository }
