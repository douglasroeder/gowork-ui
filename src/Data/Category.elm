module Data.Category exposing (Category, decoder)

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
