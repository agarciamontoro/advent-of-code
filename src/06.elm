module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
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
    | Success (List Orbit)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "/input/06.txt"
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

        Success orbits ->
            div []
                [ p []
                    [ text <|
                        "01: "
                            ++ String.fromInt (totalNumberOfOrbits orbits)
                    ]
                , p []
                    [ text <|
                        "02: "
                            ++ String.fromInt (result (parents orbits))
                    ]
                ]


type alias Children =
    List String


type alias Tree =
    Dict String Children


type alias KnownOrbits =
    Dict String Int


type alias Orbit =
    { center : String
    , inOrbit : String
    }


parseOrbit : String -> Maybe Orbit
parseOrbit text =
    case String.split ")" text of
        center :: inOrbit :: [] ->
            Just <| Orbit center inOrbit

        _ ->
            Nothing


parse : String -> List Orbit
parse text =
    List.filterMap parseOrbit (String.lines text)


addOrbit : Tree -> Orbit -> Tree
addOrbit tree orbit =
    let
        newChild : Maybe Children -> Maybe Children
        newChild mayValue =
            Just <|
                case mayValue of
                    Nothing ->
                        [ orbit.inOrbit ]

                    Just children ->
                        orbit.inOrbit :: children
    in
    Dict.update orbit.center newChild tree


initialOrbits : Tree
initialOrbits =
    Dict.singleton "COM" []


initialKnownOrbits : KnownOrbits
initialKnownOrbits =
    Dict.singleton "COM" 0


allOrbits : List Orbit -> Tree
allOrbits orbits =
    List.foldl (Utils.flip addOrbit) initialOrbits orbits


parents : List Orbit -> Dict String String
parents orbits =
    let
        insert : Orbit -> Dict String String -> Dict String String
        insert orbit dict =
            Dict.insert orbit.inOrbit orbit.center dict
    in
    List.foldl insert Dict.empty orbits


type alias NameAndNum =
    List ( String, Int )


computeKnownOrbits : Tree -> NameAndNum
computeKnownOrbits tree =
    let
        rec : NameAndNum -> NameAndNum -> NameAndNum
        rec knownOrbits toVisit =
            case toVisit of
                [] ->
                    knownOrbits

                ( parent, parentNum ) :: rest ->
                    rec
                        (( parent, parentNum ) :: knownOrbits)
                        (case Dict.get parent tree of
                            Nothing ->
                                rest

                            Just children ->
                                List.append
                                    (List.map
                                        (\n -> ( n, parentNum + 1 ))
                                        children
                                    )
                                    rest
                        )
    in
    rec [] [ ( "COM", 0 ) ]


totalNumberOfOrbits orbits =
    allOrbits orbits
        |> computeKnownOrbits
        |> List.map Tuple.second
        |> List.sum


lineage : Dict String String -> String -> List String
lineage parentsDict node =
    let
        rec currentLineage currentNode =
            case Dict.get currentNode parentsDict of
                Nothing ->
                    currentLineage

                Just parent ->
                    rec (parent :: currentLineage) parent
    in
    rec [] node


result parentsDict =
    let
        myLineage =
            lineage parentsDict "YOU"

        santasLineage =
            lineage parentsDict "SAN"

        zipped =
            Utils.zip myLineage santasLineage

        count acc list =
            case list of
                ( one, two ) :: rest ->
                    if one /= two then
                        acc

                    else
                        count (acc + 1) rest

                [] ->
                    acc

        commonPathLength =
            count 0 zipped
    in
    List.length myLineage + List.length santasLineage - 2 * commonPathLength
