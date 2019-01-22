module HOF exposing (HOF(..), fromString)


type HOF
    = Map
    | Filter
    | Reduce


fromString : String -> Maybe HOF
fromString s =
    case s of
        "Map" ->
            Just Map

        "Filter" ->
            Just Filter

        "Reduce" ->
            Just Reduce

        _ ->
            Nothing
