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
                            ++ (Tuple.second (runProgram 1 array) |> List.head |> Maybe.map String.fromInt |> Maybe.withDefault "ERR")
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ (Tuple.second (runProgram 5 array) |> List.head |> Maybe.map String.fromInt |> Maybe.withDefault "ERR")
                    ]
                ]


type ParamMode
    = Position
    | Immediate


type Instruction
    = Sum ParamMode ParamMode
    | Mul ParamMode ParamMode
    | Input
    | Output ParamMode
    | JmpTrue ParamMode ParamMode
    | JmpFalse ParamMode ParamMode
    | LessThan ParamMode ParamMode
    | Equal ParamMode ParamMode
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

        5 ->
            parseModePair JmpTrue

        6 ->
            parseModePair JmpFalse

        7 ->
            parseModePair LessThan

        8 ->
            parseModePair Equal

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


type alias InstructionOutput =
    { array : Maybe (Array Int)
    , output : Maybe Int
    , nextPosition : Maybe Int
    }


runInstruction : Int -> Array Int -> Int -> Instruction -> InstructionOutput
runInstruction input array instructionPosition instruction =
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

        JmpTrue mode1 mode2 ->
            InstructionOutput
                (Just array)
                Nothing
                (Maybe.andThen
                    (\i1 ->
                        if i1 /= 0 then
                            resolveParameter array (instructionPosition + 2) mode2

                        else
                            Just <| instructionPosition + 3
                    )
                    (resolveParameter array (instructionPosition + 1) mode1)
                )

        JmpFalse mode1 mode2 ->
            InstructionOutput
                (Just array)
                Nothing
                (Maybe.andThen
                    (\i1 ->
                        if i1 == 0 then
                            resolveParameter array (instructionPosition + 2) mode2

                        else
                            Just <| instructionPosition + 3
                    )
                    (resolveParameter array (instructionPosition + 1) mode1)
                )

        LessThan mode1 mode2 ->
            InstructionOutput
                (Maybe.map3
                    (\i1 i2 dst ->
                        Array.set dst (Utils.ifThenElse (i1 < i2) 1 0) array
                    )
                    (resolveParameter array (instructionPosition + 1) mode1)
                    (resolveParameter array (instructionPosition + 2) mode2)
                    (resolveParameter array (instructionPosition + 3) Immediate)
                )
                Nothing
                (Just <| instructionPosition + 4)

        Equal mode1 mode2 ->
            InstructionOutput
                (Maybe.map3
                    (\i1 i2 dst ->
                        Array.set dst (Utils.ifThenElse (i1 == i2) 1 0) array
                    )
                    (resolveParameter array (instructionPosition + 1) mode1)
                    (resolveParameter array (instructionPosition + 2) mode2)
                    (resolveParameter array (instructionPosition + 3) Immediate)
                )
                Nothing
                (Just <| instructionPosition + 4)

        Halt ->
            InstructionOutput (Just array) Nothing Nothing


addOutput : Maybe Int -> List Int -> List Int
addOutput maybeInt outputs =
    case maybeInt of
        Nothing ->
            outputs

        Just value ->
            value :: outputs


runProgramStep : Int -> List Int -> Array Int -> Int -> ( Array Int, List Int )
runProgramStep input outputs array readingPosition =
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
                            runProgramStep input newOutput newArray newReadingPosition
    in
    Array.get readingPosition array
        |> Maybe.andThen parseInstruction
        |> Maybe.map (runInstruction input array readingPosition)
        |> Maybe.map decideNextStep
        |> Maybe.withDefault ( Array.empty, [] )


runProgram : Int -> Array Int -> ( Array Int, List Int )
runProgram input array =
    runProgramStep input [] array 0
