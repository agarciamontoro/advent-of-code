module Utils exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, pre, text)
import Http


parseArrayInt : String -> String -> Array Int
parseArrayInt separator text =
    Array.fromList <| parseListInt separator text


parseListInt : String -> String -> List Int
parseListInt separator text =
    List.filterMap String.toInt <| String.split separator text


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


flip : (a -> b -> c) -> b -> a -> c
flip f ib ia =
    f ia ib


flippedGet : Array a -> Int -> Maybe a
flippedGet =
    flip Array.get


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


lastElement : List a -> Maybe a
lastElement l =
    List.head <| List.reverse l
