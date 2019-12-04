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
                            ++ String.fromInt numberOfValidPasswords
                    ]
                , p []
                    [ text <|
                        "02: "
                    ]
                ]


first : Int
first =
    168630


last : Int
last =
    718098


digits : Int -> List Int
digits number =
    let
        decompose acc current =
            case current // 10 of
                0 ->
                    current :: acc

                new ->
                    decompose (remainderBy 10 current :: acc) new
    in
    decompose [] number


zip : List a -> List b -> List ( a, b )
zip as_ bs_ =
    case ( as_, bs_ ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: xa, b :: xb ) ->
            ( a, b ) :: zip xa xb


pairs : List a -> List ( a, a )
pairs list =
    zip list (List.drop 1 list)


type alias Rules =
    { adjacent : Bool
    , monotony : Bool
    }


validRules : Rules -> Bool
validRules rules =
    rules.adjacent && rules.monotony


isValid : Int -> Bool
isValid number =
    let
        adjacents =
            pairs <| digits number

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


numberOfValidPasswords : Int
numberOfValidPasswords =
    List.range first last
        |> List.foldl
            (\n acc ->
                if isValid n then
                    acc + 1

                else
                    acc
            )
            0
