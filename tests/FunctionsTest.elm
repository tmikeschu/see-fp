module FunctionsTest exposing (all)

import Expect
import Functions
import SeeFpType exposing (SeeFpType(..))
import Test exposing (..)


all : Test
all =
    describe "Functions"
        [ test ".downcase downcases a StrVal value" <|
            \_ ->
                "ME"
                    |> StrVal
                    |> Functions.downcase
                    |> Expect.equal (StrVal "me")
        , test ".upcase upcases a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> Functions.upcase
                    |> Expect.equal (StrVal "ME")
        , test ".firstLetter gets the first letter of a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> Functions.firstLetter
                    |> Expect.equal (StrVal "m")
        , test ".length gets the length of a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> Functions.length
                    |> Expect.equal (IntVal 2)
        ]
