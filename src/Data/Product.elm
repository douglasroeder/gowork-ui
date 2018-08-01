module Data.Product exposing (Product, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Data.Brand as Brand


type alias Product =
    { id : Int
    , barcode : String
    , name : String
    , description : String
    , brand : Brand.Brand
    , price : Float
    , currentStock : Float
    , productType : ProductType
    }


type ProductType
    = ProductItem
    | ServiceItem


decoder : Decoder Product
decoder =
    decode Product
        |> required "id" Decode.int
        |> required "barcode" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "brand" Brand.decoder
        |> required "price" Decode.float
        |> required "current_stock" Decode.float
        |> required "product_type" productTypeDecoder


productTypeDecoder : Decoder ProductType
productTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "P" ->
                        Decode.succeed ProductItem

                    "S" ->
                        Decode.succeed ServiceItem

                    somethingElse ->
                        Decode.fail <| "Unknown contact type: " ++ somethingElse
            )
