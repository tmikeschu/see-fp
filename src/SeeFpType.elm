module SeeFpType exposing
    ( Mapper
    , Predicate
    , Reducer
    , SeeFpFunction(..)
    , SeeFpType(..)
    , acronym
    , containsR
    , difference
    , downcase
    , firstLetter
    , functionFor
    , greeting
    , group
    , increment
    , inverse
    , isEven
    , isHappy
    , isInLove
    , isLaughing
    , isOdd
    , isPerfectSquare
    , length
    , portmanteau
    , pourWater
    , product
    , realCat
    , scare
    , shorterThan4
    , square
    , startsWithH
    , sum
    , toSmiley
    , toString
    , toWord
    , upcase
    )

import Dict exposing (Dict)


type SeeFpType
    = IntVal Int
    | FloatVal Float
    | StrVal String
    | BoolVal Bool


type SeeFpFunction
    = Unary Mapper
    | Binary Reducer


type alias Mapper =
    SeeFpType -> SeeFpType


type alias Predicate =
    SeeFpType -> SeeFpType


type alias Reducer =
    SeeFpType -> Mapper


functionFor : String -> SeeFpFunction
functionFor s =
    Maybe.withDefault (Unary identity) <| Dict.get s functions


functions : Dict String SeeFpFunction
functions =
    Dict.fromList
        [ ( "acronym", Binary acronym )
        , ( "containsR", Unary containsR )
        , ( "difference", Binary difference )
        , ( "downcase", Unary downcase )
        , ( "firstLetter", Unary firstLetter )
        , ( "greeting", Binary greeting )
        , ( "group", Binary group )
        , ( "increment", Unary increment )
        , ( "inverse", Unary inverse )
        , ( "isEven", Unary isEven )
        , ( "isHappy", Unary isHappy )
        , ( "isInLove", Unary isInLove )
        , ( "isLaughing", Unary isLaughing )
        , ( "isOdd", Unary isOdd )
        , ( "isPerfectSquare", Unary isPerfectSquare )
        , ( "length", Unary length )
        , ( "portmanteau", Binary portmanteau )
        , ( "pourWater", Unary pourWater )
        , ( "product", Binary product )
        , ( "realCat", Binary realCat )
        , ( "scare", Unary scare )
        , ( "shorterThan4", Unary shorterThan4 )
        , ( "square", Unary square )
        , ( "startsWithH", Unary startsWithH )
        , ( "sum", Binary sum )
        , ( "toSmiley", Unary toSmiley )
        , ( "toWord", Unary toWord )
        , ( "upcase", Unary upcase )
        ]


toString : SeeFpType -> String
toString sft =
    case sft of
        IntVal i ->
            String.fromInt i

        FloatVal f ->
            String.slice 0 5 <| String.fromFloat f

        BoolVal b ->
            case b of
                True ->
                    "T"

                False ->
                    "F"

        StrVal s ->
            s


acronym : Reducer
acronym acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", StrVal b ) ->
            acronym (StrVal "") seefp

        ( StrVal a, StrVal b ) ->
            b
                |> String.slice 0 1
                |> String.toUpper
                |> (++) a
                |> StrVal

        _ ->
            StrVal "error"


realCat : Reducer
realCat =
    "🐈" |> (StrVal >> always >> always)


containsR : Predicate
containsR seefp =
    case seefp of
        StrVal s ->
            s
                |> String.toLower
                |> String.contains "r"
                |> BoolVal

        _ ->
            BoolVal False


difference : Reducer
difference acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", IntVal b ) ->
            IntVal <| 0 - b

        ( IntVal a, IntVal b ) ->
            IntVal <| a - b

        _ ->
            IntVal 0


downcase : Mapper
downcase seefp =
    case seefp of
        StrVal s ->
            s |> String.toLower |> StrVal

        _ ->
            StrVal "not a string"


firstLetter : Mapper
firstLetter seefp =
    case seefp of
        StrVal s ->
            s |> String.slice 0 1 |> StrVal

        _ ->
            StrVal "not a string"


greeting : Reducer
greeting acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", StrVal b ) ->
            greeting (StrVal "Hello") (StrVal b)

        ( StrVal a, StrVal b ) ->
            StrVal <| a ++ " " ++ b ++ ","

        _ ->
            StrVal "error"


group : Reducer
group acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", StrVal b ) ->
            StrVal b

        ( StrVal a, StrVal b ) ->
            StrVal <| a ++ b

        _ ->
            StrVal "error"


increment : Mapper
increment seefp =
    case seefp of
        IntVal x ->
            IntVal <| x + 1

        _ ->
            IntVal 0


inverse : Mapper
inverse seefp =
    case seefp of
        IntVal x ->
            FloatVal <| (/) 1 <| toFloat x

        _ ->
            FloatVal 0


isEven : Predicate
isEven seefp =
    case seefp of
        IntVal x ->
            BoolVal <| modBy 2 x == 0

        _ ->
            BoolVal False


isHappy : Predicate
isHappy seefp =
    case seefp of
        StrVal s ->
            BoolVal <| List.member s [ "😺", "😸", "😹", "😻" ]

        _ ->
            BoolVal False


isInLove : Predicate
isInLove seefp =
    case seefp of
        StrVal "😻" ->
            BoolVal True

        _ ->
            BoolVal False


isLaughing : Predicate
isLaughing seefp =
    case seefp of
        StrVal "😹" ->
            BoolVal True

        _ ->
            BoolVal False


isOdd : Predicate
isOdd seefp =
    case seefp of
        IntVal x ->
            BoolVal <| modBy 2 x /= 0

        _ ->
            BoolVal False


isPerfectSquare : Predicate
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


length : Mapper
length seefp =
    case seefp of
        StrVal s ->
            s |> String.length |> IntVal

        _ ->
            IntVal 0


portmanteau : Reducer
portmanteau acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", StrVal b ) ->
            portmanteau (StrVal "") seefp

        ( StrVal a, StrVal b ) ->
            b
                |> String.slice 0 3
                |> String.toLower
                |> (++) a
                |> StrVal

        _ ->
            StrVal "error"


pourWater : Mapper
pourWater =
    always (StrVal "😾")


product : Reducer
product acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", IntVal b ) ->
            IntVal <| 1 * b

        ( IntVal a, IntVal b ) ->
            IntVal <| a * b

        _ ->
            IntVal 0


scare : Mapper
scare =
    always (StrVal "🙀")


shorterThan4 : Predicate
shorterThan4 seefp =
    case seefp of
        StrVal s ->
            s
                |> String.length
                |> (>=) 4
                |> BoolVal

        _ ->
            BoolVal False


square : Mapper
square seefp =
    case seefp of
        IntVal x ->
            IntVal <| x * x

        _ ->
            IntVal 0


startsWithH : Predicate
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


sum : Reducer
sum acc seefp =
    case ( acc, seefp ) of
        ( StrVal "INIT", IntVal b ) ->
            IntVal <| b

        ( IntVal a, IntVal b ) ->
            IntVal <| a + b

        _ ->
            IntVal 0


toSmiley : Mapper
toSmiley seefp =
    let
        smileys =
            Dict.fromList <|
                [ ( "😺", "😀" )
                , ( "😸", "😄" )
                , ( "😹", "😂" )
                , ( "😻", "😍" )
                ]
    in
    case seefp of
        StrVal cat ->
            smileys
                |> Dict.get cat
                |> Maybe.withDefault "🐛"
                |> StrVal

        _ ->
            StrVal "🐛"


toWord : Mapper
toWord seefp =
    let
        words =
            Dict.fromList <|
                [ ( 4, "four" )
                , ( 8, "eight" )
                , ( 15, "fifteen" )
                , ( 16, "sixteen" )
                , ( 23, "twenty-three" )
                , ( 42, "forty-two" )
                ]
    in
    case seefp of
        IntVal x ->
            words
                |> Dict.get x
                |> Maybe.withDefault "error"
                |> StrVal

        _ ->
            StrVal "error"


upcase : Mapper
upcase seefp =
    case seefp of
        StrVal s ->
            s |> String.toUpper |> StrVal

        _ ->
            StrVal ""
