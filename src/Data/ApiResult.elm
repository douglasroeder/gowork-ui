module Data.ApiResult exposing (ApiResult, ApiErrorResult, apiDecoder, apiErrorDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias ApiResult a =
    { statusCode : Int
    , error : String
    , payload : a
    }


type alias ApiErrorResult =
    { statusCode : Int
    , error : String
    }


apiErrorDecoder : Decoder ApiErrorResult
apiErrorDecoder =
    decode ApiErrorResult
        |> required "status_code" Decode.int
        |> required "error" Decode.string


apiDecoder : Decoder a -> Decoder (ApiResult a)
apiDecoder payloadDecoder =
    decode ApiResult
        |> required "status_code" Decode.int
        |> required "error" Decode.string
        |> required "payload" payloadDecoder
