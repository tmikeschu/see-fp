module Functions exposing
    ( containsR
    , downcase
    , firstLetter
    , increment
    , inverse
    , isEven
    , isHappy
    , isInLove
    , isLaughing
    , isOdd
    , isPerfectSquare
    , length
    , shorterThan4
    , square
    , startsWithH
    , toWord
    , upcase
    )

import SeeFpType exposing (SeeFpType(..))


type alias Transformation =
    SeeFpType -> SeeFpType


containsR : Transformation
containsR seefp =
    case seefp of
        StrVal s ->
            s
                |> String.toLower
                |> String.contains "r"
                |> BoolVal

        _ ->
            BoolVal False


downcase : Transformation
downcase seefp =
    case seefp of
        StrVal s ->
            s |> String.toLower |> StrVal

        _ ->
            StrVal "not a string"


firstLetter : Transformation
firstLetter seefp =
    case seefp of
        StrVal s ->
            s |> String.slice 0 1 |> StrVal

        _ ->
            StrVal "not a string"


increment : Transformation
increment seefp =
    case seefp of
        IntVal x ->
            IntVal <| x + 1

        _ ->
            IntVal 0


inverse : Transformation
inverse seefp =
    case seefp of
        IntVal x ->
            FloatVal <| (/) 1 <| toFloat x

        _ ->
            FloatVal 0


isEven : Transformation
isEven seefp =
    case seefp of
        IntVal x ->
            BoolVal <| modBy 2 x == 0

        _ ->
            BoolVal False


isHappy : Transformation
isHappy seefp =
    case seefp of
        StrVal s ->
            BoolVal <| List.member s [ "😺", "😸", "😹", "😻" ]

        _ ->
            BoolVal False


isInLove : Transformation
isInLove seefp =
    case seefp of
        StrVal "😻" ->
            BoolVal True

        _ ->
            BoolVal False


isLaughing : Transformation
isLaughing seefp =
    case seefp of
        StrVal "😹" ->
            BoolVal True

        _ ->
            BoolVal False


isOdd : Transformation
isOdd seefp =
    case seefp of
        IntVal x ->
            BoolVal <| modBy 2 x /= 0

        _ ->
            BoolVal False


isPerfectSquare : Transformation
isPerfectSquare seefp =
    case seefp of
        IntVal x ->
            x
                |> toFloat
                |> sqrt
                |> (\b -> floor b == ceiling b)
                |> BoolVal

        _ ->
            BoolVal False


length : Transformation
length seefp =
    case seefp of
        StrVal s ->
            s |> String.length |> IntVal

        _ ->
            IntVal 0


shorterThan4 : Transformation
shorterThan4 seefp =
    case seefp of
        StrVal s ->
            s
                |> String.length
                |> (>=) 4
                |> BoolVal

        _ ->
            BoolVal False


square : Transformation
square seefp =
    case seefp of
        IntVal x ->
            IntVal <| x * x

        _ ->
            IntVal 0


startsWithH : Transformation
startsWithH seefp =
    case seefp of
        StrVal s ->
            s
                |> String.slice 0 1
                |> String.toLower
                |> (==) "h"
                |> BoolVal

        _ ->
            BoolVal False


toWord : Transformation
toWord seefp =
    case seefp of
        IntVal 4 ->
            StrVal "four"

        IntVal 8 ->
            StrVal "eight"

        IntVal 15 ->
            StrVal "fifteen"

        IntVal 16 ->
            StrVal "sixteen"

        IntVal 23 ->
            StrVal "twenty-three"

        IntVal 42 ->
            StrVal "forty-two"

        _ ->
            StrVal "error"


upcase : Transformation
upcase seefp =
    case seefp of
        StrVal s ->
            s |> String.toUpper |> StrVal

        _ ->
            StrVal ""
