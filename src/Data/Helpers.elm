module Data.Helpers exposing (dateDecoder)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)


dateDecoder : Decoder Date
dateDecoder =
    let
        convert : String -> Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    Decode.succeed date

                Err error ->
                    Decode.fail error
    in
        Decode.string |> Decode.andThen convert
