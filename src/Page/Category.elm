module Page.Category
    exposing
        ( Model
        , Msg
        , initModel
        , mount
        , update
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Data.Category exposing (Category, CategoryId, decoder)


type alias Model =
    { name : String
    , nameError : Maybe String
    , error : Maybe String
    }


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


mount : Model -> Maybe CategoryId -> ( Model, Cmd Msg )
mount model maybeId =
    case maybeId of
        Just id ->
            let
                url =
                    apiUrl ++ "/" ++ (toString id)

                req =
                    Http.get url decoder

                cmd =
                    Http.send FetchCategoryResponse req
            in
                ( initModel, cmd )

        Nothing ->
            init


type Msg
    = NameInput String
    | Submit
    | SubmitResponse (Result Http.Error Category)
    | FetchCategoryResponse (Result Http.Error Category)


apiUrl : String
apiUrl =
    "http://localhost:8080/v1/categories"


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

        SubmitResponse res ->
            case res of
                Ok category ->
                    ( initModel, Cmd.none )

                Err err ->
                    ( { model | error = Just "Error calling api" }, Cmd.none )

        FetchCategoryResponse res ->
            case res of
                Ok category ->
                    ( { model
                        | name = category.name
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = Just "Error adding category" }, Cmd.none )


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
        div []
            [ h2 [] [ text "Category" ]
            , input [ class inputClass, onInput NameInput, value model.name ] []
            , nameErrorMessage
            , button [ onClick Submit ] [ text "Save" ]
            ]
