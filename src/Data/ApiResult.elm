module Data.ApiResult exposing (ApiResult, apiDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias ApiResult a =
    { statusCode : Int
    , errors : List String
    , payload : a
    }


apiDecoder : Decoder a -> Decoder (ApiResult a)
apiDecoder payloadDecoder =
    decode ApiResult
        |> required "status_code" Decode.int
        |> required "errors" (Decode.list Decode.string)
        |> required "payload" payloadDecoder
