module Day07 exposing (..)

import Array exposing (Array)
import IntCodeComputer
import Utils


type alias Amplifier =
    Int -> Int -> Maybe Int


type alias ConfiguredAmplifier =
    Int -> Maybe Int


amplifier : Array Int -> Amplifier
amplifier program phaseSetting input =
    IntCodeComputer.init
        |> IntCodeComputer.withProgram program
        |> IntCodeComputer.withInputs [ phaseSetting, input ]
        |> IntCodeComputer.exec
        |> .outputs
        |> List.head


possibleSettings : List (List Int)
possibleSettings =
    Utils.permutations <| List.range 0 4


configureAmplifiers : Array Int -> List Int -> List ConfiguredAmplifier
configureAmplifiers program settings =
    List.map (amplifier program) settings


runConfiguredAmplifiers : List ConfiguredAmplifier -> Maybe Int
runConfiguredAmplifiers configuredAmplifiers =
    let
        f : ConfiguredAmplifier -> Maybe Int -> Maybe Int
        f nextAmplifier mayInput =
            Maybe.andThen nextAmplifier mayInput
    in
    List.foldl f (Just 0) configuredAmplifiers


highestSignal : Array Int -> Maybe Int
highestSignal program =
    let
        allSignals =
            List.map (configureAmplifiers program) possibleSettings
                |> List.map runConfiguredAmplifiers

        goodSignals =
            List.filterMap identity allSignals
    in
    if List.length allSignals == List.length goodSignals then
        List.maximum goodSignals

    else
        Nothing


solve : String -> ( String, String )
solve text =
    let
        array =
            Utils.parseArrayInt "," text

        first =
            highestSignal array
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "ERR"

        second =
            ""
    in
    ( first, second )
