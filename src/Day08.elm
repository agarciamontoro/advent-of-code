module Day08 exposing (solve)

import Array exposing (Array)
import Utils


imgWidth : Int
imgWidth =
    25


imgHeight : Int
imgHeight =
    6


imgResolution : Int
imgResolution =
    imgWidth * imgHeight


type alias Layer =
    List Int


layers : Int -> List Int -> List Layer
layers resolution pixels =
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
    if remainderBy resolution (List.length pixels) /= 0 then
        []

    else
        List.reverse <| rec [] pixels


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
    List.foldl foo ( imgResolution, 0 ) imgLayers
        |> Tuple.second


blendLayers : Layer -> Layer -> Layer
blendLayers front back =
    let
        getPixel ( f, b ) =
            Utils.ifThenElse (f /= 2) f b
    in
    List.map getPixel <| Utils.zip front back


decodeImage : List Layer -> Layer
decodeImage imageLayers =
    let
        decode currentImg remainingLayers =
            case remainingLayers of
                backLayer :: rest ->
                    decode (blendLayers currentImg backLayer) rest

                [] ->
                    currentImg
    in
    case imageLayers of
        firstLayer :: rest ->
            decode firstLayer rest

        [] ->
            []


toString : Layer -> String
toString layer =
    layers 25 layer
        |> List.map (List.map (\num -> Utils.ifThenElse (num == 1) "#" " "))
        |> List.map (String.join "")
        |> String.join "\n"


solve : String -> ( String, String )
solve text =
    let
        imgLayers =
            String.toList text
                |> List.filterMap (String.toInt << String.fromChar)
                |> layers imgResolution

        first =
            String.fromInt <| firstSolution imgLayers

        second =
            toString <| decodeImage imgLayers
    in
    ( first, second )
