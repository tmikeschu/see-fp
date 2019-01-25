module SeeFpTypeTest exposing (all)

import Expect
import SeeFpType exposing (..)
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
    describe "SeeFpType"
        [ test ".downcase downcases a StrVal value" <|
            \_ ->
                "ME"
                    |> StrVal
                    |> downcase
                    |> Expect.equal (StrVal "me")
        , test ".upcase upcases a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> upcase
                    |> Expect.equal (StrVal "ME")
        , test ".firstLetter gets the first letter of a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> firstLetter
                    |> Expect.equal (StrVal "m")
        , test ".length gets the length of a StrVal value" <|
            \_ ->
                "me"
                    |> StrVal
                    |> length
                    |> Expect.equal (IntVal 2)
        , test ".toSmiley maps a cat to a smiley" <|
            \_ ->
                cats
                    |> List.map (StrVal >> toSmiley)
                    |> Expect.equal
                        ([ "😀", "😄", "😂", "😍" ]
                            |> List.map
                                StrVal
                        )
        , test ".pourWater maps a cat to a mad cat" <|
            \_ ->
                cats
                    |> List.map (StrVal >> pourWater)
                    |> Expect.equal
                        ("😾"
                            |> List.repeat 4
                            |> List.map
                                StrVal
                        )
        , test ".scare maps a cat to a scared cat" <|
            \_ ->
                cats
                    |> List.map (StrVal >> scare)
                    |> Expect.equal
                        ("🙀"
                            |> List.repeat 4
                            |> List.map
                                StrVal
                        )
        , test ".acronym reduces names to an acronym" <|
            \_ ->
                "Hermione"
                    |> StrVal
                    |> acronym (StrVal "H.")
                    |> Expect.equal (StrVal "H.H")
        , test ".group reduces cats to a string" <|
            \_ ->
                "😾"
                    |> StrVal
                    |> group (StrVal "😹")
                    |> Expect.equal (StrVal "😹😾")
        , test ".realCat reduces catmojis to a cat" <|
            \_ ->
                "😾"
                    |> StrVal
                    |> realCat (StrVal "")
                    |> Expect.equal (StrVal "🐈")
        , test ".sum adds number to acc" <|
            \_ ->
                100
                    |> IntVal
                    |> sum (IntVal 10)
                    |> Expect.equal (IntVal 110)
        , test ".product multiplies number by acc" <|
            \_ ->
                100
                    |> IntVal
                    |> product (IntVal 2)
                    |> Expect.equal (IntVal 200)
        , test ".difference subtracts acc by number" <|
            \_ ->
                100
                    |> IntVal
                    |> difference (IntVal 250)
                    |> Expect.equal (IntVal 150)
        , test ".greeting adds name to greeting" <|
            \_ ->
                "Harry"
                    |> StrVal
                    |> greeting (StrVal "Hello")
                    |> Expect.equal (StrVal "Hello Harry,")
        , test ".portmanteau combines first three letters of names" <|
            \_ ->
                "Harry"
                    |> StrVal
                    |> portmanteau (StrVal "her")
                    |> Expect.equal (StrVal "herhar")
        ]
