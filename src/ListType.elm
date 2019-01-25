module ListType exposing
    ( ListTypeData
    , cats
    , fromString
    , listTypes
    , names
    , nums
    , people
    )

import Dict exposing (Dict)
import HOF exposing (..)
import SeeFpType exposing (SeeFpType(..))


type alias ListTypeData =
    ( String
    , { list : List SeeFpType
      , hofs : List ( String, List ( String, String ) )
      }
    )


fromString : String -> Maybe ListTypeData
fromString list =
    listTypes
        |> List.filter (Tuple.first >> (==) list)
        |> List.head


listTypes : List ListTypeData
listTypes =
    [ ( "nums"
      , { list = nums
        , hofs =
            [ ( map
              , [ ( "increment", "x => x + 1" )
                , ( "inverse", "x => 1 / x" )
                , ( "inverse", "x => 1 / x" )
                , ( "square", "x => x * x" )
                , ( "toWord", "x => custom(x)" )
                ]
              )
            , ( filter
              , [ ( "isEven", "x => x % 2 === 0" )
                , ( "isOdd", "x => x % 2 !== 0" )
                , ( "isPerfectSquare", "x => Math.sqrt(x) % 1 !== 0" )
                ]
              )
            , ( reduce
              , [ ( "sum", "(acc, x) => acc + x, 0" )
                , ( "product", "(acc, x) => acc * x, 1" )
                , ( "difference", "(acc, x) => acc - x, 0" )
                ]
              )
            ]
        }
      )
    , ( "names"
      , { list = names
        , hofs =
            [ ( map
              , [ ( "downcase", "s => s.toLowerCase()" )
                , ( "upcase", "s => s.toUpperCase()" )
                , ( "firstLetter", "s => s[0]" )
                , ( "length", "s => s.length" )
                ]
              )
            , ( filter
              , [ ( "startsWithH", "s => s[0].toLowerCase() === 'h'" )
                , ( "shorterThan4", "s => s.length < 4" )
                , ( "containsR", "s => s.toLowerCase().includes('r')" )
                ]
              )
            , ( reduce
              , [ ( "greeting", "(acc, name) => `${acc} ${name},`, 'Hello'" )
                , ( "portmanteau", "(acc, name) => `${acc}${name.substr(0, 3)}`" )
                , ( "acronym", "(acc, name) => `${acc}.${name[0].toUpperCase()}`" )
                ]
              )
            ]
        }
      )
    , ( "cats"
      , { list = cats
        , hofs =
            [ ( map
              , [ ( "pourWater", "cat => pourWater(cat)" )
                , ( "scare", "cat => scare(cat)" )
                ]
              )
            , ( filter
              , [ ( "isInLove", "cat => isInLove(cat)" )
                , ( "isLaughing", "cat => isLaughing(cat)" )
                , ( "isHappy", "cat => isHappy(cat)" )
                , ( "toSmiley", "cat => toSmiley(cat)" )
                ]
              )
            , ( reduce
              , [ ( "group", "(acc, cat) => groupCat(cat)" )
                , ( "realCat", "(acc, cat) => toCat(cat)" )
                ]
              )
            ]
        }
      )
    ]


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
