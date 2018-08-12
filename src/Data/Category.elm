module Data.Category
    exposing
        ( Category
        , CategoryId
        , CategoryList
        , APIResult
        , encoder
        , decoder
        , listDecoder
        , apiResultDecoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)


type alias CategoryId =
    Int


type alias Category =
    { id : CategoryId
    , name : String
    }


type alias CategoryList =
    List Category


type alias APIResult a =
    { statusCode : Int
    , errors : List String
    , payload : a
    }


encoder : Category -> Encode.Value
encoder category =
    Encode.object
        [ ( "name", Encode.string category.name )
        ]


apiResultDecoder : Decoder (APIResult CategoryList)
apiResultDecoder =
    decode APIResult
        |> required "status_code" Decode.int
        |> required "errors" (Decode.list Decode.string)
        |> required "payload" listDecoder


decoder : Decoder Category
decoder =
    decode Category
        |> required "id" Decode.int
        |> required "name" Decode.string


listDecoder : Decoder CategoryList
listDecoder =
    Decode.list decoder
