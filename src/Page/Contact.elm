module Page.Contact exposing (Model, Msg, initModel, mount, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Data.Contact exposing (Contact, ContactId, decoder)
import Data.ApiResult exposing (ApiResult)


type alias Model =
    { name : String
    , apiError : Maybe String
    }


initModel : Model
initModel =
    { name = ""
    , apiError = Nothing
    }


mount : Model -> Maybe ContactId -> ( Model, Cmd Msg )
mount model maybeId =
    ( initModel, fetchContact maybeId )


type Msg
    = HandleContactResponse (WebData (ApiResult Contact))


apiUrl : String
apiUrl =
    "http://localhost:8080/v1/contacts"


fetchContact : Maybe ContactId -> Cmd Msg
fetchContact maybeId =
    case maybeId of
        Just id ->
            let
                url =
                    apiUrl ++ "/" ++ (toString id)
            in
                RemoteData.Http.get url HandleContactResponse Data.Contact.decoder

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleContactResponse data ->
            case data of
                Loading ->
                    model ! []

                Success result ->
                    ( { model
                        | name = result.payload.name
                        , apiError = Nothing
                      }
                    , Cmd.none
                    )

                Failure err ->
                    { model | apiError = Just "Error calling API" } ! []

                NotAsked ->
                    model ! []


view : Model -> Html Msg
view model =
    div [ class "col-md-6" ]
        [ div [ class "bgc-white p-20 bd" ]
            [ h4 [ class "c-grey-900" ] [ text "Contact" ]
            , div [ class "mT-30" ] []
            ]
        ]
