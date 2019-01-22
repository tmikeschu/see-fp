module ListType exposing
    ( ListType(..)
    , cats
    , fromString
    , names
    , nums
    , operationsFor
    , people
    )

import SeeFpType exposing (SeeFpType(..))


type ListType
    = Nums
    | Names
    | Cats


nums : List SeeFpType
nums =
    [ 4, 8, 15, 16, 23, 42 ]
        |> List.map IntVal


names : List SeeFpType
names =
    [ "Harry", "Hermione", "Ron" ]
        |> List.map StrVal


cats : List SeeFpType
cats =
    [ "😺"
    , "😸"
    , "😹"
    , "😻"
    ]
        |> List.map StrVal


people : List SeeFpType
people =
    [ "😀"
    , "😄"
    , "😂"
    , "😍"
    ]
        |> List.map StrVal


fromString : String -> Maybe ListType
fromString list =
    case list of
        "Nums" ->
            Just Nums

        "Names" ->
            Just Names

        "Cats" ->
            Just Cats

        _ ->
            Nothing


operationsFor :
    ListType
    ->
        { map : List String
        , filter : List String
        , reduce : List String
        }
operationsFor lt =
    case lt of
        Nums ->
            { map =
                [ "increment"
                , "inverse"
                , "isEven"
                , "square"
                , "toWord"
                ]
            , filter =
                [ "isEven"
                , "isOdd"
                , "isPerfectSquare"
                ]
            , reduce = []
            }

        Names ->
            { map =
                [ "downcase"
                , "upcase"
                , "firstLetter"
                , "length"
                ]
            , filter =
                [ "startsWithH"
                , "shorterThan4"
                , "containsR"
                ]
            , reduce = []
            }

        Cats ->
            { map =
                [ "toSmiley"
                , "pourWater"
                , "scare"
                ]
            , filter =
                [ "isInLove"
                , "isLaughing"
                , "isHappy"
                ]
            , reduce = []
            }
