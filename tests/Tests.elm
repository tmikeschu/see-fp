module Tests exposing (all)

import Expect
import Factories exposing (intList, model, operation)
import Main
import SeeFpType exposing (SeeFpFunction(..), SeeFpType(..), increment)
import Test exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "Main.getCurrentElement returns n/a to start" <|
            \_ ->
                model
                    |> Main.getCurrentElement
                    |> Expect.equal "n/a"
        , test "Main.elementAt returns a maybe value (Just)" <|
            \_ ->
                intList
                    |> Main.elementAt 3
                    |> Expect.equal (Just 13)
        , test "Main.elementAt returns a maybe value (Nothing)" <|
            \_ ->
                intList
                    |> Main.elementAt 15
                    |> Expect.equal Nothing
        , test "Main.stepLeft returns the same model when no operation is set" <|
            \_ ->
                model
                    |> Main.stepLeft
                    |> Expect.equal model
        , test "Main.stepLeft pops the operation repository when operation is set" <|
            \_ ->
                model
                    |> (\m -> { m | operation = Just operation })
                    |> Main.stepLeft
                    |> .operation
                    |> Maybe.map .repository
                    |> Maybe.withDefault []
                    |> Expect.equal [ IntVal 5 ]
        ]
