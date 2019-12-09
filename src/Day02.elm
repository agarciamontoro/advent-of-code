module Day02 exposing (solve)

import Array exposing (Array)
import IntCodeComputer
import Utils


setProgram : Array Int -> Int -> Int -> Array Int
setProgram program noun verb =
    program |> Array.set 1 noun |> Array.set 2 verb


possibleInputs : List ( Int, Int )
possibleInputs =
    let
        values =
            List.range 0 99
    in
    List.concatMap (\one -> List.map (Tuple.pair one) values) values


magicNumber =
    19690720


checkResult : IntCodeComputer.ComputerState -> Bool
checkResult computer =
    case Array.get 0 computer.program of
        Nothing ->
            False

        Just value ->
            value == magicNumber


testInputsToGetMagicNumber : Array Int -> Maybe ( Int, Int )
testInputsToGetMagicNumber array =
    let
        rec : List ( Int, Int ) -> Maybe ( Int, Int )
        rec inputs =
            case inputs of
                ( noun, verb ) :: rest ->
                    let
                        computer =
                            IntCodeComputer.withProgram
                                (setProgram array noun verb)
                                IntCodeComputer.init
                    in
                    if checkResult <| IntCodeComputer.exec computer then
                        Just ( noun, verb )

                    else
                        rec rest

                [] ->
                    Nothing
    in
    rec possibleInputs


secondSolution : Array Int -> String
secondSolution array =
    case testInputsToGetMagicNumber array of
        Nothing ->
            "Error"

        Just ( noun, verb ) ->
            String.fromInt (100 * noun + verb)


solve : String -> ( String, String )
solve text =
    let
        array =
            Utils.parseArrayInt "," text

        firstComputer =
            IntCodeComputer.init
                |> IntCodeComputer.withProgram (setProgram array 12 2)

        first =
            IntCodeComputer.exec firstComputer
                |> .program
                |> Array.get 0
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "ERR"

        second =
            secondSolution array
    in
    ( first, second )
