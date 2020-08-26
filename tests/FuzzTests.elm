module FuzzTests exposing (..)

import Array exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import MyList exposing (..)
import Random exposing (maxInt, minInt)
import Test exposing (..)


addOneTests : Test
addOneTests =
    describe "addOne"
        [ fuzz int "adds 1 to any integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , test "when 1 is added to 2, the result is 3" <|
            \_ ->
                addOne 2 |> Expect.equal 3
        , fuzz2 int int "adds two given integers" <|
            \num1 num2 ->
                add num1 num2 |> Expect.equal (num1 + num2)
        , fuzzWith { runs = 200 } int "adds 1 to the given integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        , fuzz (intRange minInt maxInt) "adds 1 to the given integer range" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        ]


flipTests : Test
flipTests =
    describe "flip"
        [ fuzz bool "negates the given boolean value" <|
            \value ->
                flip value |> Expect.equal (not value)
        ]


arrayGetTests : Test
arrayGetTests =
    describe "Array.get"
        [ fuzz (array <| intRange -20 20) "returns Nothing for out of range index" <|
            \intArray ->
                let
                    length =
                        Array.length intArray
                in
                intArray
                    |> Array.get length
                    |> Expect.equal Nothing
        ]


myListSumTests : Test
myListSumTests =
    describe "My List sum"
        [ test "is 0 when empty" <|
            \_ ->
                sum Empty |> Expect.equal 0
        , test "can sum values" <|
            \_ ->
                let
                    node =
                        Node 3 <| Node 2 <| Node 1 Empty
                in
                sum node |> Expect.equal 6
        , fuzz3 int int int "can sum random values" <|
            \n1 n2 n3 ->
                let
                    node =
                        Node n1 <| Node n2 <| Node n3 Empty
                in
                sum node |> Expect.equal (n1 + n2 + n3)
        ]


addOne : Int -> Int
addOne x =
    x + 1


add : Int -> Int -> Int
add x y =
    x + y


flip : Bool -> Bool
flip =
    not
