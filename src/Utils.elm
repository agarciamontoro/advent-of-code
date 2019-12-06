module Utils exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http


parseArrayInt : String -> Array Int
parseArrayInt text =
    Array.fromList <| parseListInt text


parseListInt : String -> List Int
parseListInt text =
    List.filterMap String.toInt <| String.split "," text


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


mayCons : Maybe a -> List a -> List a
mayCons maybeInt outputs =
    case maybeInt of
        Nothing ->
            outputs

        Just value ->
            value :: outputs


zip : List a -> List b -> List ( a, b )
zip as_ bs_ =
    case ( as_, bs_ ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: xa, b :: xb ) ->
            ( a, b ) :: zip xa xb
