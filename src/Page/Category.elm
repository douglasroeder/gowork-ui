module Page.Category
    exposing
        ( Model
        , Msg
        , initModel
        , mount
        , update
        , view
        )

import Data.ApiResult exposing (ApiResult)
import Data.Category exposing (Category, CategoryId, decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Rest exposing (apiEndpoint)
import Route exposing (Route(..))


type alias Model =
    { name : String
    , nameError : Maybe String
    , apiError : Maybe String
    }


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , apiError = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


mount : Model -> Maybe CategoryId -> ( Model, Cmd Msg )
mount model maybeId =
    ( initModel, fetchCategory maybeId )


type Msg
    = NameInput String
    | Submit
    | Cancel
    | SubmitResponse (Result Http.Error (ApiResult Category))
    | HandleCategoryResponse (WebData (ApiResult Category))


apiUrl : String
apiUrl =
    apiEndpoint ++ "/v1/categories"


fetchCategory : Maybe CategoryId -> Cmd Msg
fetchCategory maybeId =
    case maybeId of
        Just id ->
            let
                url =
                    apiUrl ++ "/" ++ (toString id)
            in
                RemoteData.Http.get url HandleCategoryResponse Data.Category.decoder

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )

        Submit ->
            let
                isValid =
                    model.name /= ""
            in
                if isValid then
                    ( { model
                        | nameError = Nothing
                      }
                    , submitForm model.name
                    )
                else
                    ( { model
                        | nameError = Just "name cannot be blank!"
                      }
                    , Cmd.none
                    )

        Cancel ->
            model ! [ Route.goto CategoriesRoute ]

        SubmitResponse res ->
            case res of
                Ok category ->
                    ( initModel, Route.goto CategoriesRoute )

                Err err ->
                    ( { model | apiError = Just "Error calling api" }, Cmd.none )

        HandleCategoryResponse data ->
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

                Failure error ->
                    { model | apiError = Just "Error calling API" } ! []

                NotAsked ->
                    model ! []


submitForm : String -> Cmd Msg
submitForm name =
    let
        category =
            Category 0 name

        req =
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Content-Type" "application/json"
                    ]
                , url = apiUrl
                , body = Http.jsonBody (Data.Category.encoder category)
                , expect = Http.expectJson Data.Category.decoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send SubmitResponse <| req


renderError : Maybe String -> Html Msg
renderError maybeError =
    case maybeError of
        Just error ->
            div [ class "alert alert-danger" ]
                [ text error ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    let
        ( inputClass, nameErrorMessage ) =
            case model.nameError of
                Just error ->
                    ( "form-control is-invalid"
                    , (div [ class "invalid-feedback" ] [ text error ])
                    )

                Nothing ->
                    ( "form-control"
                    , text ""
                    )
    in
        div [ class "col-md-6" ]
            [ renderError model.apiError
            , div [ class "bgc-white p-20 bd" ]
                [ h4 [ class "c-grey-900" ] [ text "Category" ]
                , div [ class "mT-30" ]
                    [ div [ class "form-group" ]
                        [ label [ for "name" ] [ text "Name" ]
                        , input [ class inputClass, id "name", onInput NameInput, value model.name ] []
                        , nameErrorMessage
                        ]
                    , button [ class "btn btn-primary", onClick Submit ] [ text "Save" ]
                    , button [ class "btn btn-default", onClick Cancel ] [ text "Cancel" ]
                    ]
                ]
            ]
