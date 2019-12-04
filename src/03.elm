module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http
import Set



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
    | Success ( List Direction, List Direction )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/input/03.txt"
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

        Success ( oneWire, anotherWire ) ->
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (solveOne oneWire anotherWire)
                    ]
                , p []
                    [ text <|
                        "02: "
                    ]
                ]


type Direction
    = Up Int
    | Down Int
    | Right Int
    | Left Int


parseDirection : String -> Maybe Direction
parseDirection str =
    let
        parseTuple tuple =
            case tuple of
                ( _, Nothing ) ->
                    Nothing

                ( 'U', Just n ) ->
                    Just <| Up n

                ( 'D', Just n ) ->
                    Just <| Down n

                ( 'R', Just n ) ->
                    Just <| Right n

                ( 'L', Just n ) ->
                    Just <| Left n

                ( _, Just _ ) ->
                    Nothing
    in
    String.uncons str
        |> Maybe.map (Tuple.mapSecond String.toInt)
        |> Maybe.andThen parseTuple


parseLine : String -> List Direction
parseLine line =
    List.filterMap parseDirection <| String.split "," line


parse : String -> ( List Direction, List Direction )
parse text =
    case Debug.log "text: " <| String.lines text of
        one :: two :: empty :: [] ->
            ( parseLine one, parseLine two )

        one :: [] ->
            ( parseLine one, [] )

        _ ->
            ( [], [] )


type alias Coordinate =
    { lat : Int
    , lon : Int
    }


fromTuple : ( Int, Int ) -> Coordinate
fromTuple t =
    Coordinate (Tuple.first t) (Tuple.second t)


toTuple : Coordinate -> ( Int, Int )
toTuple c =
    ( c.lat, c.lon )


toString : Coordinate -> String
toString c =
    "(" ++ String.fromInt c.lat ++ ", " ++ String.fromInt c.lon ++ ")"


genList : a -> Int -> (a -> a) -> List a
genList first num f =
    let
        rec : List a -> a -> Int -> List a
        rec current last count =
            if count >= num then
                current

            else
                rec (f last :: current) (f last) (count + 1)
    in
    rec [] first 0 |> List.reverse


plusOne : Int -> Int
plusOne n =
    n + 1


minusOne : Int -> Int
minusOne n =
    n - 1


zip : List a -> List b -> List ( a, b )
zip as_ bs_ =
    case ( as_, bs_ ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: xa, b :: xb ) ->
            ( a, b ) :: zip xa xb


genSide : Coordinate -> Direction -> List Coordinate
genSide first direction =
    case direction of
        Up n ->
            List.map fromTuple <|
                zip (genList first.lat n plusOne) (List.repeat n first.lon)

        Down n ->
            List.map fromTuple <|
                zip (genList first.lat n minusOne) (List.repeat n first.lon)

        Right n ->
            List.map fromTuple <|
                zip (List.repeat n first.lat) (genList first.lon n plusOne)

        Left n ->
            List.map fromTuple <|
                zip (List.repeat n first.lat) (genList first.lon n minusOne)


lastElement : List a -> Maybe a
lastElement l =
    List.head <| List.reverse l


generateWire : List Direction -> List Coordinate
generateWire directions =
    let
        foo : Direction -> List Coordinate -> List Coordinate
        foo direction acc =
            case lastElement acc of
                Just last ->
                    List.append acc (genSide last direction)

                Nothing ->
                    genSide (Coordinate 0 0) direction
    in
    List.foldl foo [] directions


manhattanDistance : ( Int, Int ) -> Int
manhattanDistance ( a, b ) =
    abs a + abs b


distanceFromClosest : List Coordinate -> List Coordinate -> Int
distanceFromClosest one two =
    Set.intersect (Set.fromList (List.map toTuple one)) (Set.fromList (List.map toTuple two))
        |> Set.map manhattanDistance
        |> Set.toList
        |> List.minimum
        |> Maybe.withDefault -1


solveOne : List Direction -> List Direction -> Int
solveOne one two =
    distanceFromClosest (generateWire one) (generateWire two)
