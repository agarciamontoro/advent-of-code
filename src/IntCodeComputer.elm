module IntCodeComputer exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Utils


type ParamMode
    = Position
    | Immediate
    | Relative


type OutParamMode
    = OutPosition
    | OutRelative


type Instruction
    = Sum ParamMode ParamMode OutParamMode
    | Mul ParamMode ParamMode OutParamMode
    | Input OutParamMode
    | Output ParamMode
    | JmpTrue ParamMode ParamMode
    | JmpFalse ParamMode ParamMode
    | LessThan ParamMode ParamMode OutParamMode
    | Equal ParamMode ParamMode OutParamMode
    | Rebase ParamMode
    | Halt


type alias ComputerState =
    { inputs : List Int
    , outputs : List Int
    , ip : Int
    , program : Array Int
    , memory : Dict Int Int
    , relativeBase : Int
    , finished : Bool
    , error : Maybe String
    }


init : ComputerState
init =
    ComputerState [] [] 0 Array.empty Dict.empty 0 False Nothing


withInputs : List Int -> ComputerState -> ComputerState
withInputs inputs computer =
    { computer | inputs = inputs }


withProgram : Array Int -> ComputerState -> ComputerState
withProgram program computer =
    { computer | program = program }


parseMode : Int -> Maybe ParamMode
parseMode number =
    case number of
        0 ->
            Just Position

        1 ->
            Just Immediate

        2 ->
            Just Relative

        _ ->
            Nothing


parseOutMode : Int -> Maybe OutParamMode
parseOutMode number =
    case number of
        0 ->
            Just OutPosition

        2 ->
            Just OutRelative

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
                    Maybe.map2 instruction (parseMode one) (parseMode two)

                one :: [] ->
                    Maybe.map (\m -> instruction m Position) (parseMode one)

                _ ->
                    Nothing

        parseModePairAndOut : (ParamMode -> ParamMode -> OutParamMode -> Instruction) -> Maybe Instruction
        parseModePairAndOut instruction =
            case paramModesList of
                one :: two :: three :: rest ->
                    Maybe.map3
                        instruction
                        (parseMode one)
                        (parseMode two)
                        (parseOutMode three)

                one :: two :: rest ->
                    Maybe.map2 (\o t -> instruction o t OutPosition) (parseMode one) (parseMode two)

                one :: [] ->
                    Maybe.map (\m -> instruction m Position OutPosition) (parseMode one)

                _ ->
                    Nothing
    in
    case opCode of
        1 ->
            parseModePairAndOut Sum

        2 ->
            parseModePairAndOut Mul

        3 ->
            List.head paramModesList
                |> Maybe.andThen parseOutMode
                |> Maybe.map Input

        4 ->
            List.head paramModesList
                |> Maybe.andThen parseMode
                |> Maybe.map Output

        5 ->
            parseModePair JmpTrue

        6 ->
            parseModePair JmpFalse

        7 ->
            parseModePairAndOut LessThan

        8 ->
            parseModePairAndOut Equal

        9 ->
            List.head paramModesList
                |> Maybe.andThen parseMode
                |> Maybe.map Rebase

        99 ->
            Just Halt

        _ ->
            Nothing


resolveParameter : Int -> ComputerState -> ParamMode -> Maybe Int
resolveParameter position computer mode =
    case mode of
        Immediate ->
            Array.get position computer.program

        Position ->
            Array.get position computer.program
                |> Maybe.andThen (getValue computer)

        Relative ->
            Array.get position computer.program
                |> Maybe.andThen
                    (\p -> getValue computer (computer.relativeBase + p))


resolveOutParameter : Int -> ComputerState -> OutParamMode -> Maybe Int
resolveOutParameter position computer mode =
    case mode of
        OutPosition ->
            Array.get position computer.program

        OutRelative ->
            Array.get position computer.program
                |> Maybe.map (\p -> computer.relativeBase + p)


runInstruction : ComputerState -> Maybe Instruction -> ComputerState
runInstruction computer instruction =
    case instruction of
        Just (Sum mode1 mode2 outmode) ->
            case
                Maybe.map3
                    (\i1 i2 dst -> setValue dst (i1 + i2) computer)
                    (resolveParameter (computer.ip + 1) computer mode1)
                    (resolveParameter (computer.ip + 2) computer mode2)
                    (resolveOutParameter (computer.ip + 3) computer outmode)
            of
                Nothing ->
                    finishError "Error while running a Sum instruction" computer

                Just newState ->
                    { newState | ip = computer.ip + 4 }

        Just (Mul mode1 mode2 outmode) ->
            case
                Maybe.map3
                    (\i1 i2 dst -> setValue dst (i1 * i2) computer)
                    (resolveParameter (computer.ip + 1) computer mode1)
                    (resolveParameter (computer.ip + 2) computer mode2)
                    (resolveOutParameter (computer.ip + 3) computer outmode)
            of
                Nothing ->
                    finishError "Error while running a Mul instruction" computer

                Just newState ->
                    { newState | ip = computer.ip + 4 }

        Just (Input outMode) ->
            case computer.inputs of
                input :: rest ->
                    case
                        resolveOutParameter (computer.ip + 1) computer outMode
                            |> Maybe.map (\dst -> setValue dst input computer)
                    of
                        Nothing ->
                            finishError "Instruction pointer < 0" computer

                        Just newState ->
                            { newState | ip = computer.ip + 2, inputs = rest }

                [] ->
                    finishError "Input list is empty." computer

        Just (Output mode) ->
            case resolveParameter (computer.ip + 1) computer mode of
                Nothing ->
                    finishError "Error resolving parameter" computer

                Just newOutput ->
                    { computer
                        | outputs = newOutput :: computer.outputs
                        , ip = computer.ip + 2
                    }

        Just (JmpTrue mode1 mode2) ->
            case resolveParameter (computer.ip + 1) computer mode1 of
                Nothing ->
                    finishError "Error resolving parameter" computer

                Just i1 ->
                    if i1 /= 0 then
                        case resolveParameter (computer.ip + 2) computer mode2 of
                            Nothing ->
                                finishError "Error resolving parameter" computer

                            Just i2 ->
                                { computer | ip = i2 }

                    else
                        { computer | ip = computer.ip + 3 }

        Just (JmpFalse mode1 mode2) ->
            case resolveParameter (computer.ip + 1) computer mode1 of
                Nothing ->
                    finishError "Error resolving parameter" computer

                Just i1 ->
                    if i1 == 0 then
                        case resolveParameter (computer.ip + 2) computer mode2 of
                            Nothing ->
                                finishError "Error resolving parameter" computer

                            Just i2 ->
                                { computer | ip = i2 }

                    else
                        { computer | ip = computer.ip + 3 }

        Just (LessThan mode1 mode2 outMode) ->
            case
                Maybe.map3
                    (\i1 i2 dst ->
                        setValue dst (Utils.ifThenElse (i1 < i2) 1 0) computer
                    )
                    (resolveParameter (computer.ip + 1) computer mode1)
                    (resolveParameter (computer.ip + 2) computer mode2)
                    (resolveOutParameter (computer.ip + 3) computer outMode)
            of
                Nothing ->
                    finishError "Error while running a LessThan instruction" computer

                Just newState ->
                    { newState | ip = computer.ip + 4 }

        Just (Equal mode1 mode2 outMode) ->
            case
                Maybe.map3
                    (\i1 i2 dst ->
                        setValue dst (Utils.ifThenElse (i1 == i2) 1 0) computer
                    )
                    (resolveParameter (computer.ip + 1) computer mode1)
                    (resolveParameter (computer.ip + 2) computer mode2)
                    (resolveOutParameter (computer.ip + 3) computer outMode)
            of
                Nothing ->
                    finishError "Error while running a Equal instruction" computer

                Just newState ->
                    { newState | ip = computer.ip + 4 }

        Just (Rebase mode) ->
            case resolveParameter (computer.ip + 1) computer mode of
                Nothing ->
                    finishError "Error resolving parameter" computer

                Just offset ->
                    { computer
                        | relativeBase = computer.relativeBase + offset
                        , ip = computer.ip + 2
                    }

        Just Halt ->
            { computer | finished = True }

        Nothing ->
            finishError "Error reading instruction" computer


{-| Gets the value from the program, defaulting to the external memory if the
position is not available. Returns Nothing only if `position` is negative.
-}
getValue : ComputerState -> Int -> Maybe Int
getValue computer position =
    if position < 0 then
        Nothing

    else
        case Array.get position computer.program of
            Nothing ->
                Just <| Maybe.withDefault 0 (Dict.get position computer.memory)

            Just number ->
                Just number


{-| Sets the value to the program, defaulting to the external memory if the
position is not available. Sets the computer state to finished with error when
`position` is negative.
-}
setValue : Int -> Int -> ComputerState -> ComputerState
setValue position value computer =
    if position < 0 then
        finishError "setValue: Negative position" computer

    else if position < Array.length computer.program then
        { computer | program = Array.set position value computer.program }

    else
        { computer | memory = Dict.insert position value computer.memory }


finishError : String -> ComputerState -> ComputerState
finishError msg computer =
    { computer | finished = True, error = Just msg }


getInstruction : ComputerState -> Maybe Instruction
getInstruction computer =
    Array.get computer.ip computer.program
        |> Maybe.andThen parseInstruction


step : ComputerState -> ComputerState
step computer =
    runInstruction computer (getInstruction computer)


exec : ComputerState -> ComputerState
exec computer =
    if computer.finished then
        computer

    else
        exec (step computer)


consumeFirstOutput : ComputerState -> ( ComputerState, Maybe Int )
consumeFirstOutput computer =
    case ( computer.outputs, computer.finished ) of
        ( [], True ) ->
            ( computer, Nothing )

        ( [], False ) ->
            consumeFirstOutput (step computer)

        ( output :: rest, _ ) ->
            ( { computer | outputs = rest }, Just output )


addInput : Int -> ComputerState -> ComputerState
addInput input computer =
    { computer | inputs = List.append computer.inputs [ input ] }
