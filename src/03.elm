module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http
import Set
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
            let
                solution =
                    solve oneWire anotherWire
            in
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (Tuple.first solution)
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ String.fromInt (Tuple.second solution)
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


plusOne : Int -> Int
plusOne n =
    n + 1


minusOne : Int -> Int
minusOne n =
    n - 1


genSide : Coordinate -> Direction -> List Coordinate
genSide first direction =
    case direction of
        Up n ->
            List.map fromTuple <|
                Utils.zip (Utils.genList first.lat n plusOne) (List.repeat n first.lon)

        Down n ->
            List.map fromTuple <|
                Utils.zip (Utils.genList first.lat n minusOne) (List.repeat n first.lon)

        Right n ->
            List.map fromTuple <|
                Utils.zip (List.repeat n first.lat) (Utils.genList first.lon n plusOne)

        Left n ->
            List.map fromTuple <|
                Utils.zip (List.repeat n first.lat) (Utils.genList first.lon n minusOne)


generateWire : List Direction -> List Coordinate
generateWire directions =
    let
        foo : Direction -> List Coordinate -> List Coordinate
        foo direction acc =
            case Utils.lastElement acc of
                Just last ->
                    List.append acc (genSide last direction)

                Nothing ->
                    genSide (Coordinate 0 0) direction
    in
    List.foldl foo [] directions


manhattanDistance : ( Int, Int ) -> Int
manhattanDistance ( a, b ) =
    abs a + abs b


numberOfSteps : List ( Int, ( Int, Int ) ) -> ( Int, Int ) -> Int
numberOfSteps list coord =
    case List.head (List.filter (\x -> Tuple.second x == coord) list) of
        Nothing ->
            -1

        Just ( idx, ( lat, lon ) ) ->
            idx


distanceFromClosestAndMinSteps : List Coordinate -> List Coordinate -> ( Int, Int )
distanceFromClosestAndMinSteps one two =
    let
        oneTuple =
            List.map toTuple one

        oneTupleIndexed =
            List.indexedMap Tuple.pair oneTuple

        twoTuple =
            List.map toTuple two

        twoTupleIndexed =
            List.indexedMap Tuple.pair twoTuple

        intersections =
            Set.intersect (Set.fromList oneTuple) (Set.fromList twoTuple)

        intersectionsList =
            Set.toList intersections

        oneSteps =
            List.map (numberOfSteps oneTupleIndexed) intersectionsList

        twoSteps =
            List.map (numberOfSteps twoTupleIndexed) intersectionsList

        minSteps =
            2
                + Maybe.withDefault -3
                    (Utils.zip oneSteps twoSteps
                        |> List.map (\x -> Tuple.first x + Tuple.second x)
                        |> List.minimum
                    )
    in
    ( Set.map manhattanDistance intersections
        |> Set.toList
        |> List.minimum
        |> Maybe.withDefault -1
    , minSteps
    )


solve : List Direction -> List Direction -> ( Int, Int )
solve one two =
    distanceFromClosestAndMinSteps (generateWire one) (generateWire two)
