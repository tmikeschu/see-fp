module Operation exposing
    ( Operation
    , fromString
    , pop
    , push
    )

import SeeFpType exposing (SeeFpType(..))


type alias Operation =
    { name : String
    , repository : List SeeFpType
    , signature : String
    , fn : SeeFpType -> SeeFpType
    }


type alias Update =
    Operation -> Operation


popList : List a -> List a
popList =
    Maybe.withDefault []
        << Maybe.map List.reverse
        << List.tail
        << List.reverse


pop : Update
pop op =
    { op | repository = popList op.repository }


pushList : a -> List a -> List a
pushList x xs =
    xs ++ [ x ]


push : SeeFpType -> Update
push x op =
    { op | repository = pushList (op.fn x) op.repository }


fromString : String -> Maybe Operation
fromString o =
    case o of
        "increment" ->
            Just <|
                Operation
                    "increment"
                    []
                    "x => x + 1"
                    (\seefp ->
                        case seefp of
                            IntVal x ->
                                IntVal <| x + 1

                            _ ->
                                IntVal 0
                    )

        "inverse" ->
            Just <|
                Operation
                    "inverse"
                    []
                    "x => 1 / x"
                    (\seefp ->
                        case seefp of
                            IntVal x ->
                                FloatVal <| (/) 1 <| toFloat x

                            _ ->
                                FloatVal 0
                    )

        "isEven" ->
            Just <|
                Operation
                    "isEven"
                    []
                    "x => x % 2 === 0"
                    (\seefp ->
                        case seefp of
                            IntVal x ->
                                BoolVal <| modBy 2 x == 0

                            _ ->
                                BoolVal False
                    )

        "square" ->
            Just <|
                Operation
                    "square"
                    []
                    "x => x * x"
                    (\seefp ->
                        case seefp of
                            IntVal x ->
                                IntVal <| x * x

                            _ ->
                                IntVal 0
                    )

        "toWord" ->
            Just <|
                Operation
                    "toWord"
                    []
                    "x => custom(x)"
                    (\seefp ->
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
                    )

        _ ->
            Nothing
