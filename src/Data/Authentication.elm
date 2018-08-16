module Data.Authentication exposing (Authentication, decoder)

import Data.ApiResult exposing (ApiResult, apiDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Authentication =
    { token : String }


decoder : Decoder (ApiResult Authentication)
decoder =
    apiDecoder authenticationDecoder


authenticationDecoder : Decoder Authentication
authenticationDecoder =
    decode Authentication
        |> optional "token" Decode.string ""
