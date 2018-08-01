module Data.Brand exposing (Brand, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Brand =
    { id : Int
    , name : String
    }


decoder : Decoder Brand
decoder =
    decode Brand
        |> required "id" Decode.int
        |> required "name" Decode.string
