module Data.Category exposing (Category, encoder, decoder, listDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)


type alias Category =
    { id : Int
    , name : String
    }


encoder : Category -> Encode.Value
encoder category =
    Encode.object
        [ ( "name", Encode.string category.name )
        ]


decoder : Decoder Category
decoder =
    decode Category
        |> required "id" Decode.int
        |> required "name" Decode.string


listDecoder : Decoder (List Category)
listDecoder =
    Decode.list decoder
