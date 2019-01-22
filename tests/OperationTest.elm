module OperationTest exposing (all)

import Expect
import Functions exposing (..)
import Operation
import SeeFpType exposing (SeeFpType(..))
import Test exposing (..)


all : Test
all =
    describe "Operations"
        [ test "operations is a list of (name, signature, function)" <|
            \_ ->
                Expect.equal Operation.operations
                    [ ( "increment", "x => x + 1", increment )
                    , ( "inverse", "x => 1 / x", inverse )
                    , ( "inverse", "x => 1 / x", inverse )
                    , ( "isEven", "x => x % 2 === 0", isEven )
                    , ( "isOdd", "x => x % 2 !== 0", isOdd )
                    , ( "isPerfectSquare", "x => Math.sqrt(x) % 1 !== 0", isPerfectSquare )
                    , ( "square", "x => x * x", square )
                    , ( "toWord", "x => custom(x)", toWord )
                    , ( "startsWithH", "s => s[0].toLowerCase() === 'h'", startsWithH )
                    , ( "shorterThan4", "s => s.length < 4", shorterThan4 )
                    , ( "containsR", "s => s.toLowerCase().includes('r')", containsR )
                    , ( "isInLove", "cat => isInLove(cat)", isInLove )
                    , ( "isLaughing", "cat => isLaughing(cat)", isLaughing )
                    , ( "isHappy", "cat => isHappy(cat)", isHappy )
                    , ( "downcase", "s => s.toLowerCase()", downcase )
                    , ( "upcase", "s => s.toUpperCase()", upcase )
                    , ( "firstLetter", "s => s[0]", firstLetter )
                    , ( "length", "s => s.length", length )
                    , ( "toSmiley", "cat => toSmiley(cat)", toSmiley )
                    , ( "pourWater", "cat => pourWater(cat)", pourWater )
                    , ( "scare", "cat => scare(cat)", scare )
                    ]
        ]
