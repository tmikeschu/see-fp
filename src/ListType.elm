module ListType exposing
    ( ListType(..)
    , cats
    , listTypeFromString
    , names
    , nums
    , operationsFor
    , people
    )


type ListType
    = Nums
    | Names
    | Cats


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


operationsFor : ListType -> List String
operationsFor lt =
    case lt of
        Nums ->
            [ "increment" ]

        Names ->
            []

        Cats ->
            []
