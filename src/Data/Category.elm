module Data.Category
    exposing
        ( Category
        , CategoryId
        , CategoryList
        , encoder
        , decoder
        , listDecoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)
import Data.ApiResult exposing (ApiResult, apiDecoder)


type alias CategoryId =
    Int


type alias Category =
    { id : CategoryId
    , name : String
    }


type alias CategoryList =
    List Category


encoder : Category -> Encode.Value
encoder category =
    Encode.object
        [ ( "name", Encode.string category.name )
        ]


listDecoder : Decoder (ApiResult CategoryList)
listDecoder =
    apiDecoder (Decode.list categoryDecoder)


decoder : Decoder (ApiResult Category)
decoder =
    apiDecoder categoryDecoder


categoryDecoder : Decoder Category
categoryDecoder =
    decode Category
        |> required "id" Decode.int
        |> required "name" Decode.string
