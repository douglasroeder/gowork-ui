module Data.Contact exposing (Contact, decoder)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Data.Helpers exposing (dateDecoder)


type alias Contact =
    { id : Int
    , name : String
    , email : String
    , landPhone : String
    , mobilePhone : String
    , cnpjCpf : String
    , ieRg : String
    , contactType : ContactType
    , address : String
    , suburb : String
    , postalCode : String
    , city : String
    , state : State
    , countryCode : String
    , createdAt : Date
    }


type alias State =
    { id : Int
    , name : String
    , slug : String
    }


type ContactType
    = Individual
    | Company


decoder : Decoder Contact
decoder =
    decode Contact
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "email" Decode.string
        |> required "land_phone" Decode.string
        |> required "mobile_phone" Decode.string
        |> required "cnpj_cpf" Decode.string
        |> required "ie_rg" Decode.string
        |> required "contact_type" contactTypeDecoder
        |> required "address" Decode.string
        |> required "suburb" Decode.string
        |> required "postal_code" Decode.string
        |> required "city" Decode.string
        |> required "state" stateDecoder
        |> required "country_code" Decode.string
        |> required "created_at" dateDecoder


stateDecoder : Decoder State
stateDecoder =
    decode State
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "slug" Decode.string


contactTypeDecoder : Decoder ContactType
contactTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "PF" ->
                        Decode.succeed Individual

                    "PJ" ->
                        Decode.succeed Company

                    somethingElse ->
                        Decode.fail <| "Unknown contact type: " ++ somethingElse
            )
