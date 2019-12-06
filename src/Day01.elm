module Day01 exposing (solve)

import Utils


fuelFromMass : Int -> Int
fuelFromMass mass =
    mass // 3 - 2


fuelSum : List Int -> Int
fuelSum list =
    List.sum <| List.map fuelFromMass list


totalFuel : List Int -> Int
totalFuel list =
    List.sum <| List.map totalFuelPerModule list


totalFuelPerModule : Int -> Int
totalFuelPerModule mass =
    fuelFromMass mass |> additionalFuel


additionalFuel : Int -> Int
additionalFuel mass =
    let
        moreFuel =
            fuelFromMass mass
    in
    if moreFuel <= 0 then
        mass

    else
        mass + additionalFuel moreFuel


solve : String -> ( String, String )
solve text =
    let
        list =
            Utils.parseListInt "\n" text

        first =
            String.fromInt (fuelSum list)

        second =
            String.fromInt (totalFuel list)
    in
    ( first, second )
