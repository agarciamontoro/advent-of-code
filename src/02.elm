module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http



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
                            ++ String.fromInt (Maybe.withDefault -999 (Array.get 0 (runWholeProgram array)))
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ String.fromInt 5
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


type alias OpParams =
    { src1 : Int
    , src2 : Int
    , dst : Int
    }


flippedGet : Array a -> Int -> Maybe a
flippedGet arr pos =
    Array.get pos arr


readOpParams : Array Int -> Int -> Maybe OpParams
readOpParams array opPosition =
    Maybe.map3 OpParams
        (Array.get (opPosition + 1) array |> Maybe.andThen (flippedGet array))
        (Array.get (opPosition + 2) array |> Maybe.andThen (flippedGet array))
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
                    runProgramStep (Debug.log "newArray: " newArray) (currentPos + 4)
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


runWholeProgram : Array Int -> Array Int
runWholeProgram array =
    let
        initArray =
            array |> Array.set 1 12 |> Array.set 2 2
    in
    runProgramStep (Debug.log "array" initArray) 0
