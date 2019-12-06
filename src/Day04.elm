module Day04 exposing (solve)

import Utils


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


solve : String -> ( String, String )
solve _ =
    let
        one =
            String.fromInt (numberOfValidPasswords isValidOne)

        second =
            String.fromInt (numberOfValidPasswords isValidTwo)
    in
    ( one, second )
