module RippleCarryAdderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Http exposing (Expect)
import RippleCarryAdder exposing (..)
import Test exposing (..)


inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \_ ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                inverter 1
                    |> Expect.equal 0
        ]


orGateTests =
    describe "Or Game"
        [ test "output is 1 when the input are 1 and 0" <|
            \_ ->
                orGate 1 0
                    |> Expect.equal 1
        ]


fullAdderTests =
    describe "Full adder"
        [ describe "when both inputs are 0"
            [ test "and carry-in is 0 too, then both sum and carry-out are 0" <|
                \_ ->
                    fullAdder 0 0 0
                        |> Expect.equal { carry = 0, sum = 0 }
            , test "but carry-in is 1, then sum is 1 and carry-out is 0" <|
                \_ ->
                    fullAdder 0 0 1
                        |> Expect.equal { carry = 0, sum = 1 }
            ]
        , describe "when the 1st input is 0, and the 2nd input is 1"
            [ test "and carry-in is 0, then sum is 1 and carry-out is 0" <|
                \_ ->
                    fullAdder 0 1 0
                        |> Expect.equal { carry = 0, sum = 1 }
            , test "and carry-in is 1, then sum is 0 and carry-out is 1" <|
                \_ ->
                    fullAdder 0 1 1
                        |> Expect.equal { carry = 1, sum = 0 }
            ]
        , describe "when the 1st input is 1, and the 2nd input is 0"
            [ test "and carry-in is 0, then sum is 1 and carry-out is 0" <|
                \_ ->
                    fullAdder 1 0 0
                        |> Expect.equal { carry = 0, sum = 1 }
            , test "and carry-in is 1, then sum is 0 and carry-out is 1" <|
                \_ ->
                    fullAdder 1 0 1
                        |> Expect.equal { carry = 1, sum = 0 }
            ]
        , describe "when the 1st input is 1, and the 2nd input is 1"
            [ test "and carry-in is 0, then sum is 0 and carry-out is 1" <|
                \_ ->
                    fullAdder 1 1 0
                        |> Expect.equal { carry = 1, sum = 0 }
            , test "and carry-in is 1, then sum is 1 and carry-out is 1" <|
                \_ ->
                    fullAdder 1 1 1
                        |> Expect.equal { carry = 1, sum = 1 }
            ]
        ]
