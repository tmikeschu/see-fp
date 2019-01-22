module Operation exposing
    ( Operation
    , filter
    , fromString
    , pop
    , push
    )

import Functions exposing (..)
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
    let
        val =
            op.fn x
    in
    { op | repository = pushList val op.repository }


filter : SeeFpType -> Update
filter x op =
    let
        val =
            op.fn x
    in
    case val of
        BoolVal b ->
            { op
                | repository =
                    if b then
                        pushList x op.repository

                    else
                        op.repository
            }

        _ ->
            op


operations :
    List
        ( String
        , String
        , SeeFpType
          -> SeeFpType
        )
operations =
    [ ( "increment", "x => x + 1", increment )
    , ( "inverse", "x => 1 / x", inverse )
    , ( "inverse", "x => 1 / x", inverse )
    , ( "isEven", "x => x % 2 === 0", isEven )
    , ( "isOdd", "x => x % 2 !== 0", isOdd )
    , ( "isPerfectSquare", "x => Math.sqrt(x) % 1 !== 0", isPerfectSquare )
    , ( "square", "x => x * x", square )
    , ( "toWord", "x => custom(x)", toWord )
    , ( "startsWithH", "s => s[0].toLowerCase() === 'h'", startsWithH )
    , ( "shorterThan4", "s => s.length < 4", shorterThan4 )
    , ( "containsR", "s => s.toLowerCase().includes('r')", containsR )
    , ( "isInLove", "cat => isInLove(cat)", isInLove )
    , ( "isLaughing", "cat => isLaughing(cat)", isLaughing )
    , ( "isHappy", "cat => isHappy(cat)", isHappy )
    ]


fromString : String -> Maybe Operation
fromString o =
    operations
        |> List.filter ((\( x, _, _ ) -> x) >> (==) o)
        |> List.head
        |> Maybe.map (\( a, b, c ) -> Operation a [] b c)
