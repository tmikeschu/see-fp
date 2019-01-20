module SeeFpType exposing (SeeFpType(..), toString)


type SeeFpType
    = IntVal Int
    | FloatVal Float
    | StrVal String
    | BoolVal Bool


toString : SeeFpType -> String
toString sft =
    case sft of
        IntVal i ->
            String.fromInt i

        FloatVal f ->
            String.slice 0 5 <| String.fromFloat f

        BoolVal b ->
            case b of
                True ->
                    "T"

                False ->
                    "F"

        StrVal s ->
            s
