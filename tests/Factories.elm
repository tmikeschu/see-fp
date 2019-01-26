module Factories exposing (intList, model, operation)

import Main
import Operation exposing (Operation)
import SeeFpType exposing (SeeFpFunction(..), SeeFpType(..), increment)


model : Main.Model
model =
    Tuple.first Main.init


operation : Operation
operation =
    Operation
        "increment"
        [ IntVal 5, IntVal 10 ]
        "x => x + 1"
        (Unary increment)


intList : List Int
intList =
    List.range 10 20
