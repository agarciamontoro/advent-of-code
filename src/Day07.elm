module Day07 exposing (..)

import Array exposing (Array)
import IntCodeComputer as ICC exposing (ComputerState)
import Utils


type alias Amplifier =
    Int -> Int -> Maybe Int


type alias ConfiguredAmplifier =
    Int -> Maybe Int


amplifier : Array Int -> Amplifier
amplifier program phaseSetting input =
    ICC.init
        |> ICC.withProgram program
        |> ICC.withInputs [ phaseSetting, input ]
        |> ICC.exec
        |> .outputs
        |> List.head


possibleSettingsLinear : List (List Int)
possibleSettingsLinear =
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


highestSignalLinear : Array Int -> Maybe Int
highestSignalLinear program =
    let
        allSignals =
            List.map (configureAmplifiers program) possibleSettingsLinear
                |> List.map runConfiguredAmplifiers

        goodSignals =
            List.filterMap identity allSignals
    in
    if List.length allSignals == List.length goodSignals then
        List.maximum goodSignals

    else
        Nothing


possibleSettingsFeedback : List (List Int)
possibleSettingsFeedback =
    Utils.permutations <| List.range 5 9


initAmplifierFeedback : Array Int -> Int -> ComputerState
initAmplifierFeedback program phaseSetting =
    ICC.init
        |> ICC.withProgram program
        |> ICC.withInputs [ phaseSetting ]


configureAmplifiersFeedback : Array Int -> List Int -> List ComputerState
configureAmplifiersFeedback program settings =
    List.map (initAmplifierFeedback program) settings


runFeedback : Int -> Maybe Int -> List ComputerState -> Maybe Int
runFeedback input lastOutput amplifiers =
    case amplifiers of
        firstAmplifier :: rest ->
            let
                ( computer, output ) =
                    ICC.addInput input firstAmplifier
                        |> ICC.consumeFirstOutput
            in
            if computer.finished then
                lastOutput

            else
                Maybe.andThen
                    (\i -> runFeedback i output (List.append rest [ computer ]))
                    output

        [] ->
            lastOutput


highestSignalFeedback : Array Int -> Maybe Int
highestSignalFeedback program =
    let
        allSignals =
            possibleSettingsFeedback
                |> List.map (configureAmplifiersFeedback program)
                |> List.map (runFeedback 0 Nothing)

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
            highestSignalLinear array
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "ERR"

        second =
            highestSignalFeedback array
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "ERR"
    in
    ( first, second )
