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
        { url = "/input/05.txt"
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
            ( Success (parse fullText), Cmd.none )

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
                            ++ (Tuple.second (runProgram array) |> List.map String.fromInt |> String.join ", ")
                    ]
                , p []
                    [ text <|
                        "02: "
                    ]
                ]


parse : String -> Array Int
parse text =
    let
        stringList =
            String.split "," text

        intList =
            List.filterMap String.toInt stringList
    in
    Array.fromList intList


type ParamMode
    = Position
    | Immediate


type Instruction
    = Sum ParamMode ParamMode
    | Mul ParamMode ParamMode
    | Input
    | Output ParamMode
    | Halt


parseMode : Int -> Maybe ParamMode
parseMode number =
    case number of
        0 ->
            Just Position

        1 ->
            Just Immediate

        _ ->
            Nothing


parseInstruction : Int -> Maybe Instruction
parseInstruction number =
    let
        opCode =
            remainderBy 100 number

        paramModesList =
            List.reverse <| Utils.digits (number // 100)

        parseModePair : (ParamMode -> ParamMode -> Instruction) -> Maybe Instruction
        parseModePair instruction =
            case paramModesList of
                one :: two :: rest ->
                    Maybe.map2 instruction
                        (parseMode one)
                        (parseMode two)

                one :: [] ->
                    Maybe.map (\m -> instruction m Position) (parseMode one)

                _ ->
                    Nothing
    in
    case opCode of
        1 ->
            parseModePair Sum

        2 ->
            parseModePair Mul

        3 ->
            Just Input

        4 ->
            List.head paramModesList
                |> Maybe.andThen parseMode
                |> Maybe.map Output

        99 ->
            Just Halt

        _ ->
            Nothing


resolveParameter : Array Int -> Int -> ParamMode -> Maybe Int
resolveParameter program readingPosition mode =
    case mode of
        Immediate ->
            Array.get readingPosition program

        Position ->
            Array.get readingPosition program
                |> Maybe.andThen (\pos -> Array.get pos program)


input : Int
input =
    1


type alias InstructionOutput =
    { array : Maybe (Array Int)
    , output : Maybe Int
    , nextPosition : Maybe Int
    }


runInstruction : Array Int -> Int -> Instruction -> InstructionOutput
runInstruction array instructionPosition instruction =
    case instruction of
        Sum mode1 mode2 ->
            InstructionOutput
                (Maybe.map3 (\i1 i2 dst -> Array.set dst (i1 + i2) array)
                    (resolveParameter array (instructionPosition + 1) mode1)
                    (resolveParameter array (instructionPosition + 2) mode2)
                    (resolveParameter array (instructionPosition + 3) Immediate)
                )
                Nothing
                (Just <| instructionPosition + 4)

        Mul mode1 mode2 ->
            InstructionOutput
                (Maybe.map3 (\i1 i2 dst -> Array.set dst (i1 * i2) array)
                    (resolveParameter array (instructionPosition + 1) mode1)
                    (resolveParameter array (instructionPosition + 2) mode2)
                    (resolveParameter array (instructionPosition + 3) Immediate)
                )
                Nothing
                (Just <| instructionPosition + 4)

        Input ->
            InstructionOutput
                (Array.get (instructionPosition + 1) array
                    |> Maybe.map (\dst -> Array.set dst input array)
                )
                Nothing
                (Just <| instructionPosition + 2)

        Output mode ->
            InstructionOutput
                (Just array)
                (resolveParameter array (instructionPosition + 1) mode)
                (Just <| instructionPosition + 2)

        Halt ->
            InstructionOutput (Just array) Nothing Nothing


addOutput : Maybe Int -> List Int -> List Int
addOutput maybeInt outputs =
    case maybeInt of
        Nothing ->
            outputs

        Just value ->
            value :: outputs


runProgramStep : List Int -> Array Int -> Int -> ( Array Int, List Int )
runProgramStep outputs array readingPosition =
    let
        decideNextStep : InstructionOutput -> ( Array Int, List Int )
        decideNextStep out =
            case out.array of
                Nothing ->
                    ( Array.empty, outputs )

                Just newArray ->
                    let
                        newOutput =
                            addOutput out.output outputs
                    in
                    case out.nextPosition of
                        Nothing ->
                            ( newArray, newOutput )

                        Just newReadingPosition ->
                            runProgramStep newOutput newArray newReadingPosition
    in
    Array.get readingPosition array
        |> Maybe.andThen parseInstruction
        |> Maybe.map (runInstruction array readingPosition)
        |> Maybe.map decideNextStep
        |> Maybe.withDefault ( Array.empty, [] )


runProgram : Array Int -> ( Array Int, List Int )
runProgram array =
    runProgramStep [] array 0
