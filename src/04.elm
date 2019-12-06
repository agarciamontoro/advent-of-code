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
    | Success


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok fullText) ->
            ( Success, Cmd.none )

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

        Success ->
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (numberOfValidPasswords isValidOne)
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ String.fromInt (numberOfValidPasswords isValidTwo)
                    ]
                ]


first : Int
first =
    168630


last : Int
last =
    718098


pairs : List a -> List ( a, a )
pairs list =
    Utils.zip list (List.drop 1 list)


type alias Four =
    { one : Int, two : Int, three : Int, four : Int }


preprocess : Int -> List Four
preprocess number =
    let
        rec acc remaining =
            case remaining of
                one :: two :: three :: four :: rest ->
                    rec (Four one two three four :: acc) (List.drop 1 remaining)

                _ ->
                    acc
    in
    rec [] (-1 :: Utils.digits number ++ [ 10 ])


type alias Rules =
    { adjacent : Bool
    , monotony : Bool
    }


validRules : Rules -> Bool
validRules rules =
    rules.adjacent && rules.monotony


isValidOne : Int -> Bool
isValidOne number =
    let
        adjacents =
            pairs <| Utils.digits number

        foo : ( Int, Int ) -> Rules -> Rules
        foo ( one, two ) rules =
            if not rules.monotony then
                rules

            else if rules.adjacent then
                Rules True (one <= two)

            else
                Rules (one == two) (one <= two)
    in
    List.foldl foo (Rules False True) adjacents |> validRules


isValidTwo : Int -> Bool
isValidTwo number =
    let
        foo : Four -> Rules -> Rules
        foo four rules =
            if not rules.monotony then
                rules

            else if rules.adjacent then
                Rules True (four.two <= four.three)

            else
                Rules
                    (four.one /= four.two && four.two == four.three && four.three /= four.four)
                    (four.two <= four.three)
    in
    List.foldl foo (Rules False True) (preprocess number) |> validRules


numberOfValidPasswords : (Int -> Bool) -> Int
numberOfValidPasswords isValid =
    List.range first last
        |> List.foldl
            (\n acc ->
                if isValid n then
                    acc + 1

                else
                    acc
            )
            0
