module Playground exposing (main)

import Html
import Regex


escapeEarth : Float -> Float -> String
escapeEarth myVelocity mySpeed =
    if myVelocity > 11.186 then
        "GodSpeed"

    else if mySpeed == 7.67 then
        "Stay in orbit"

    else
        "Come back"


computeSpeed : Float -> Float -> Float
computeSpeed distance time =
    distance / time


computeTime : Float -> Float -> Float
computeTime startTime endTime =
    endTime - startTime


main =
    computeTime 2 3
        |> computeSpeed 7.67
        |> escapeEarth 11
        |> Html.text
