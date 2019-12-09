module Main exposing (..)

import Array exposing (Array)
import Browser
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day08
import Dict exposing (Dict)
import Html exposing (Html, div, h3, li, p, pre, text, ul)
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


type State
    = Failure
    | Success ( String, String )


type alias Model =
    { solutions : Dict Int State }


notImplemented : String -> ( String, String )
notImplemented _ =
    ( "Not", "Implemented" )


allSolvers : Array (String -> ( String, String ))
allSolvers =
    Array.fromList
        [ Day01.solve
        , Day02.solve
        , Day03.solve
        , Day04.solve
        , Day05.solve
        , Day06.solve
        , notImplemented
        , Day08.solve
        ]


solver : Int -> (String -> ( String, String ))
solver num =
    Array.get (num - 1) allSolvers
        |> Maybe.withDefault (\s -> ( "Not", "implemented." ))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        buildCommands num =
            let
                fileName =
                    String.padLeft 2 '0' (String.fromInt num)
            in
            Http.get
                { url = "/input/" ++ fileName ++ ".txt"
                , expect = Http.expectString (GotInput num)
                }
    in
    ( Model Dict.empty
    , List.range 1 (Array.length allSolvers)
        |> List.map buildCommands
        |> Cmd.batch
    )



-- UPDATE


type Msg
    = GotInput Int (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput number (Ok fullText) ->
            ( Model <|
                Dict.insert
                    number
                    (Success <| solver number fullText)
                    model.solutions
            , Cmd.none
            )

        GotInput number (Err _) ->
            ( Model <| Dict.insert number Failure model.solutions, Cmd.none )



-- VIEW


viewSolution : ( Int, State ) -> Html Msg
viewSolution ( day, state ) =
    case state of
        Failure ->
            text "Malformed input"

        Success ( sol1, sol2 ) ->
            div []
                [ h3 [] [ text <| "Day " ++ String.fromInt day ]
                , ul []
                    [ li [] [ pre [] [ text <| sol1 ] ]
                    , li [] [ pre [] [ text <| sol2 ] ]
                    ]
                ]


view : Model -> Html Msg
view model =
    div [] <|
        List.map
            viewSolution
            (Dict.toList model.solutions)
