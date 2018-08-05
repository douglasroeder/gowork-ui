module Data.Category exposing (Category, decoder, listDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Category =
    { id : Int
    , name : String
    }


decoder : Decoder Category
decoder =
    decode Category
        |> required "id" Decode.int
        |> required "name" Decode.string


listDecoder : Decoder (List Category)
listDecoder =
    Decode.list decoder
