module RippleCarryAdder exposing (Binary, andGate, fullAdder, halfAdder, inverter, orGate, rippleCarryAdder)

import Bitwise

andGate a b =
    Bitwise.and a b


orGate a b =
    Bitwise.or a b


inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


rippleCarryAdder a b carryIn =
    let
        firstResult =
            fullAdder a.d3 b.d3 carryIn

        secondResult =
            fullAdder a.d2 b.d2 firstResult.carry

        thirdResult =
            fullAdder a.d1 b.d1 secondResult.carry

        finalResult =
            fullAdder a.d0 b.d0 thirdResult.carry
    in
    { carry = finalResult.carry
    , sum0 = finalResult.sum
    , sum1 = thirdResult.sum
    , sum2 = secondResult.sum
    , sum3 = firstResult.sum
    }
