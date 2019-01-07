module Tests exposing (all)

import Expect
import Main
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Main.getCurrentElement returns n/a to start" <|
            \_ ->
                Main.init
                    |> Tuple.first
                    |> Main.getCurrentElement
                    |> Expect.equal "n/a"
        ]
