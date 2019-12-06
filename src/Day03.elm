module Day03 exposing (solve)

import Set
import Utils


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
    case String.lines text of
        one :: two :: empty :: [] ->
            ( parseLine one, parseLine two )

        one :: [] ->
            ( parseLine one, [] )

        _ ->
            ( [], [] )


type alias Coordinate =
    ( Int, Int )


genSide : Coordinate -> Direction -> List Coordinate
genSide first direction =
    case direction of
        Up n ->
            Utils.genList
                first
                n
                (\c -> ( Tuple.first c + 1, Tuple.second first ))

        Down n ->
            Utils.genList
                first
                n
                (\c -> ( Tuple.first c - 1, Tuple.second first ))

        Right n ->
            Utils.genList
                first
                n
                (\c -> ( Tuple.first first, Tuple.second c + 1 ))

        Left n ->
            Utils.genList
                first
                n
                (\c -> ( Tuple.first first, Tuple.second c - 1 ))


generateWire : List Direction -> List Coordinate
generateWire directions =
    let
        foo : Direction -> List Coordinate -> List Coordinate
        foo direction acc =
            case Utils.lastElement acc of
                Just last ->
                    List.append acc (genSide last direction)

                Nothing ->
                    genSide ( 0, 0 ) direction
    in
    List.foldl foo [] directions


manhattanDistance : Coordinate -> Int
manhattanDistance ( a, b ) =
    abs a + abs b


numberOfSteps : List ( Int, Coordinate ) -> Coordinate -> Int
numberOfSteps list coord =
    case List.head (List.filter (\x -> Tuple.second x == coord) list) of
        Nothing ->
            -1

        Just ( idx, ( lat, lon ) ) ->
            idx


distanceFromClosestAndMinSteps : List Coordinate -> List Coordinate -> ( Int, Int )
distanceFromClosestAndMinSteps one two =
    let
        oneIndexed =
            List.indexedMap Tuple.pair one

        twoIndexed =
            List.indexedMap Tuple.pair two

        oneSet =
            Set.fromList one

        intersections =
            List.filter (\c -> Set.member c oneSet) two

        oneSteps =
            List.map (numberOfSteps oneIndexed) intersections

        twoSteps =
            List.map (numberOfSteps twoIndexed) intersections

        minSteps =
            2
                + Maybe.withDefault -3
                    (Utils.zip oneSteps twoSteps
                        |> List.map (\x -> Tuple.first x + Tuple.second x)
                        |> List.minimum
                    )
    in
    ( List.map manhattanDistance intersections
        |> List.minimum
        |> Maybe.withDefault -1
    , minSteps
    )


solve : String -> ( String, String )
solve text =
    let
        wires =
            parse text

        firstWire =
            generateWire (Tuple.first wires)

        secondWire =
            generateWire (Tuple.second wires)

        solution =
            distanceFromClosestAndMinSteps firstWire secondWire
    in
    Tuple.mapBoth String.fromInt String.fromInt solution
