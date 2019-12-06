module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http
import Utils



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }



-- MODEL


type Model
    = Loading
    | Failure
    | Success (Array Int)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/input/02.txt"
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok fullText) ->
            ( Success (Utils.parseArrayInt "," fullText), Cmd.none )

        GotText (Err _) ->
            ( Failure, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Waiting for input"

        Failure ->
            text "Malformed input"

        Success array ->
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (Maybe.withDefault -999 (Array.get 0 (runWholeProgram array 12 2)))
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ printInputsAndAnswer array
                    ]
                ]


type alias OpParams =
    { src1 : Int
    , src2 : Int
    , dst : Int
    }


readOpParams : Array Int -> Int -> Maybe OpParams
readOpParams array opPosition =
    Maybe.map3 OpParams
        (Array.get (opPosition + 1) array |> Maybe.andThen (Utils.flippedGet array))
        (Array.get (opPosition + 2) array |> Maybe.andThen (Utils.flippedGet array))
        (Array.get (opPosition + 3) array)


updateArraySum : Array Int -> OpParams -> Array Int
updateArraySum array params =
    Array.set params.dst (params.src1 + params.src2) array


updateArrayMul : Array Int -> OpParams -> Array Int
updateArrayMul array params =
    Array.set params.dst (params.src1 * params.src2) array


runProgramStep : Array Int -> Int -> Array Int
runProgramStep array readingPosition =
    let
        nextStep : Int -> Maybe (Array Int) -> Array Int
        nextStep currentPos maybeNewArray =
            case maybeNewArray of
                Nothing ->
                    Array.empty

                Just newArray ->
                    runProgramStep newArray (currentPos + 4)
    in
    case Array.get readingPosition array of
        Nothing ->
            Array.empty

        Just 1 ->
            readOpParams array readingPosition
                |> Maybe.map (updateArraySum array)
                |> nextStep readingPosition

        Just 2 ->
            readOpParams array readingPosition
                |> Maybe.map (updateArrayMul array)
                |> nextStep readingPosition

        Just 99 ->
            array

        Just _ ->
            Array.empty


runWholeProgram : Array Int -> Int -> Int -> Array Int
runWholeProgram array noun verb =
    let
        initArray =
            array |> Array.set 1 noun |> Array.set 2 verb
    in
    runProgramStep initArray 0


possibleInputs : List ( Int, Int )
possibleInputs =
    let
        values =
            List.range 0 99
    in
    List.concatMap (\one -> List.map (Tuple.pair one) values) values


checkResult : Array Int -> Bool
checkResult array =
    case Array.get 0 array of
        Nothing ->
            False

        Just value ->
            value == 19690720


testInputsToGet19690720 : Array Int -> Maybe ( Int, Int )
testInputsToGet19690720 array =
    let
        foo : ( Int, Int ) -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
        foo newInput inputFound =
            case ( inputFound, newInput ) of
                ( Just input, _ ) ->
                    Just input

                ( Nothing, ( noun, verb ) ) ->
                    if runWholeProgram array noun verb |> checkResult then
                        Just ( noun, verb )

                    else
                        Nothing
    in
    List.foldl foo Nothing possibleInputs


printInputsAndAnswer : Array Int -> String
printInputsAndAnswer array =
    case testInputsToGet19690720 array of
        Nothing ->
            "Error"

        Just ( noun, verb ) ->
            String.fromInt noun ++ ", " ++ String.fromInt verb ++ " -> " ++ String.fromInt (100 * noun + verb)
