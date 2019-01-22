module ListTypeTests exposing (all)

import Expect
import ListType
import Test exposing (..)


all : Test
all =
    describe "ListType"
        [ test ".operationsFor Nums returns record of hofs and operation list of strings" <|
            \_ ->
                ListType.Nums
                    |> ListType.operationsFor
                    |> Expect.equal
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
        , test ".operationsFor Names returns record of hofs and operation list of strings" <|
            \_ ->
                ListType.Names
                    |> ListType.operationsFor
                    |> Expect.equal
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
        , test ".operationsFor Cats returns record of hofs and operation list of strings" <|
            \_ ->
                ListType.Cats
                    |> ListType.operationsFor
                    |> Expect.equal
                        { map = []
                        , filter =
                            [ "isInLove"
                            , "isLaughing"
                            , "isHappy"
                            ]
                        , reduce = []
                        }
        ]
