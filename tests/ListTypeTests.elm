module ListTypeTests exposing (all)

import Expect
import ListType
import Test exposing (..)


all : Test
all =
    describe "ListType"
        [ test ".operationsFor returns list of operationn strings" <|
            \_ ->
                ListType.Nums
                    |> ListType.operationsFor
                    |> Expect.equal [ "increment" ]
        ]
