module Page.Contacts exposing (Model, Msg, initModel, mount, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Data.Contact exposing (Contact, ContactList)
import Data.ApiResult exposing (ApiResult)
import Route exposing (Route(..))


type alias Model =
    { contacts : WebData (ApiResult ContactList)
    , error : Maybe String
    }


initModel : Model
initModel =
    { contacts = Loading
    , error = Nothing
    }


mount : Model -> ( Model, Cmd Msg )
mount model =
    ( initModel, fetchContacts )


type Msg
    = FetchContacts
    | HandleContactsResponse (WebData (ApiResult ContactList))
    | ClickAddContact


apiUrl : String
apiUrl =
    "http://localhost:8080/v1/contacts"


fetchContacts : Cmd Msg
fetchContacts =
    RemoteData.Http.get apiUrl HandleContactsResponse Data.Contact.listDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchContacts ->
            model ! [ fetchContacts ]

        HandleContactsResponse data ->
            { model | contacts = data } ! []

        ClickAddContact ->
            model ! [ Route.goto ContactAddRoute ]


view : Model -> Html Msg
view model =
    div [ class "col-md-12" ]
        [ renderApiError model.error
        , div [ class "bgc-white bd bdrs-3 p-20" ]
            [ div [ class "peers ai-c" ]
                [ div [ class "peer peer-greed" ]
                    [ h4 [ class "c-grey-900 mB-20" ] [ text "Contacts" ] ]
                , div [ class "peer" ]
                    [ button [ class "btn btn-primary", onClick ClickAddContact ] [ text "Add" ] ]
                ]
            , div [ class "mT-30" ]
                [ renderContacts model.contacts ]
            ]
        ]


renderApiError : Maybe String -> Html Msg
renderApiError maybeError =
    case maybeError of
        Just error ->
            div [ class "alert alert-danger" ] [ text error ]

        Nothing ->
            text ""


renderContacts : WebData (ApiResult ContactList) -> Html Msg
renderContacts data =
    case data of
        Loading ->
            text "Fetching contacts..."

        Success result ->
            result.payload
                |> List.map contact
                |> tbody []
                |> (\r -> contactsHeader :: [ r ])
                |> table [ class "table table-hover" ]

        Failure error ->
            text "Failed to fetch data"

        NotAsked ->
            text ""


contact : Contact -> Html Msg
contact { id, name } =
    tr []
        [ td [] [ text (toString id) ]
        , td [] [ text name ]
        , td []
            [ a [ class "btn btn-default" ] [ text "edit" ]
            ]
        ]


contactsHeader : Html Msg
contactsHeader =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] []
            ]
        ]
