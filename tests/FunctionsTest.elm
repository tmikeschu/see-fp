module FunctionsTest exposing (all)

import Expect
import Functions
import SeeFpType exposing (SeeFpType(..))
import Test exposing (..)


all : Test
all =
    let
        cats =
            [ "😺"
            , "😸"
            , "😹"
            , "😻"
            ]
    in
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
        , test ".toSmiley maps a cat to a smiley" <|
            \_ ->
                cats
                    |> List.map (StrVal >> Functions.toSmiley)
                    |> Expect.equal
                        ([ "😀", "😄", "😂", "😍" ]
                            |> List.map
                                StrVal
                        )
        , test ".pourWater maps a cat to a mad cat" <|
            \_ ->
                cats
                    |> List.map (StrVal >> Functions.pourWater)
                    |> Expect.equal
                        ("😾"
                            |> List.repeat 4
                            |> List.map
                                StrVal
                        )
        , test ".scare maps a cat to a scared cat" <|
            \_ ->
                cats
                    |> List.map (StrVal >> Functions.scare)
                    |> Expect.equal
                        ("🙀"
                            |> List.repeat 4
                            |> List.map
                                StrVal
                        )
        ]
