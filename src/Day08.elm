module Day08 exposing (solve)

import Array exposing (Array)
import Utils


width : Int
width =
    25


heigth : Int
heigth =
    6


resolution : Int
resolution =
    width * heigth


type alias Layer =
    List Int


layers : List Int -> List Layer
layers pixels =
    let
        rec currLayers remainingPixels =
            case remainingPixels of
                [] ->
                    currLayers

                remaining ->
                    rec
                        (List.take resolution remainingPixels :: currLayers)
                        (List.drop resolution remainingPixels)
    in
    if remainderBy (width * heigth) (List.length pixels) /= 0 then
        []

    else
        Debug.log "Layers: " <| rec [] pixels


numDigits : Int -> Layer -> Int
numDigits digit layer =
    List.length <| List.filter ((==) digit) layer


firstSolution : List Layer -> Int
firstSolution imgLayers =
    let
        foo layer ( bestNumZeros, bestSolution ) =
            let
                currNumZeros =
                    numDigits 0 layer
            in
            if currNumZeros < bestNumZeros then
                ( currNumZeros, numDigits 1 layer * numDigits 2 layer )

            else
                ( bestNumZeros, bestSolution )
    in
    List.foldl foo ( resolution, 0 ) imgLayers
        |> Tuple.second


solve : String -> ( String, String )
solve text =
    let
        imgLayers =
            String.toList text
                |> List.filterMap (String.toInt << String.fromChar)
                |> layers

        first =
            String.fromInt <| firstSolution imgLayers
    in
    ( first, "" )
