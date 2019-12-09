module Day09 exposing (solve)

import Array exposing (Array)
import IntCodeComputer
import Utils


solve : String -> ( String, String )
solve text =
    let
        array =
            Utils.parseArrayInt "," text

        computer inputs =
            IntCodeComputer.init
                |> IntCodeComputer.withProgram array
                |> IntCodeComputer.withInputs inputs

        first =
            IntCodeComputer.exec (computer [ 1 ])
                |> .outputs
                |> List.map String.fromInt
                |> String.join ", "

        second =
            IntCodeComputer.exec (computer [ 2 ])
                |> .outputs
                |> List.map String.fromInt
                |> String.join ", "
    in
    ( first, second )
