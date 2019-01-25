module ListTypeTests exposing (all)

import Dict exposing (Dict)
import Expect
import ListType
import Test exposing (..)


all : Test
all =
    describe "ListType"
        [ test ".listTypes returns Nums, Names, and Cats pointing to map/filter/reduce" <|
            \_ ->
                ListType.listTypes
                    |> Expect.equal
                        [ ( "nums"
                          , { list = ListType.nums
                            , hofs =
                                [ ( "map"
                                  , [ ( "increment", "x => x + 1" )
                                    , ( "inverse", "x => 1 / x" )
                                    , ( "inverse", "x => 1 / x" )
                                    , ( "square", "x => x * x" )
                                    , ( "toWord", "x => custom(x)" )
                                    ]
                                  )
                                , ( "filter"
                                  , [ ( "isEven", "x => x % 2 === 0" )
                                    , ( "isOdd", "x => x % 2 !== 0" )
                                    , ( "isPerfectSquare", "x => Math.sqrt(x) % 1 !== 0" )
                                    ]
                                  )
                                , ( "reduce"
                                  , [ ( "sum", "(acc, x) => acc + x, 0" )
                                    , ( "product", "(acc, x) => acc * x, 1" )
                                    , ( "difference", "(acc, x) => acc - x, 0" )
                                    ]
                                  )
                                ]
                            }
                          )
                        , ( "names"
                          , { list = ListType.names
                            , hofs =
                                [ ( "map"
                                  , [ ( "downcase", "s => s.toLowerCase()" )
                                    , ( "upcase", "s => s.toUpperCase()" )
                                    , ( "firstLetter", "s => s[0]" )
                                    , ( "length", "s => s.length" )
                                    ]
                                  )
                                , ( "filter"
                                  , [ ( "startsWithH", "s => s[0].toLowerCase() === 'h'" )
                                    , ( "shorterThan4", "s => s.length < 4" )
                                    , ( "containsR", "s => s.toLowerCase().includes('r')" )
                                    ]
                                  )
                                , ( "reduce"
                                  , [ ( "greeting", "(acc, name) => `${acc} ${name},`, 'Hello'" )
                                    , ( "portmanteau", "(acc, name) => `${acc}${name.substr(0, 3)}`" )
                                    , ( "acronym", "(acc, name) => `${acc}.${name[0].toUpperCase()}`" )
                                    ]
                                  )
                                ]
                            }
                          )
                        , ( "cats"
                          , { list = ListType.cats
                            , hofs =
                                [ ( "map"
                                  , [ ( "pourWater", "cat => pourWater(cat)" )
                                    , ( "scare", "cat => scare(cat)" )
                                    ]
                                  )
                                , ( "filter"
                                  , [ ( "isInLove", "cat => isInLove(cat)" )
                                    , ( "isLaughing", "cat => isLaughing(cat)" )
                                    , ( "isHappy", "cat => isHappy(cat)" )
                                    , ( "toSmiley", "cat => toSmiley(cat)" )
                                    ]
                                  )
                                , ( "reduce"
                                  , [ ( "group", "(acc, cat) => groupCat(cat)" )
                                    , ( "realCat", "(acc, cat) => toCat(cat)" )
                                    ]
                                  )
                                ]
                            }
                          )
                        ]
        ]
