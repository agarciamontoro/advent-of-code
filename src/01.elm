module Main exposing (..)

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
    | Success (List Int)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/input/01.txt"
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

        Success list ->
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (fuelSum list)
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ String.fromInt (totalFuel list)
                    ]
                ]


parse : String -> List Int
parse text =
    let
        stringList =
            String.split "\n" text

        intList =
            List.filterMap String.toInt stringList
    in
    intList


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
