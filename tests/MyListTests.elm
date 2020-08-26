module MyListTests exposing (..)

import Expect exposing (Expectation)
import MyList exposing (..)
import Test exposing (..)


myListSum : Test
myListSum =
    describe "MyList sum"
        [ test "MyList should be able to sum" <|
            \_ -> sum (Node 1 (Node 2 (Node 3 Empty))) |> Expect.equal 6
        , test "MyList should sum to 0 for empty" <|
            \_ -> sum Empty |> Expect.equal 0
        ]


myListEmpty : Test
myListEmpty =
    describe "MyList empty"
        [ test "MyList should be empty when Empty" <|
            \_ -> isEmpty Empty |> Expect.true "should be empty"
        , test "MyList should not be empty when Node" <|
            \_ -> isEmpty (Node 1 Empty) |> Expect.false "should not be empty"
        ]
