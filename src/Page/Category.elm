module Page.Category exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Data.Category exposing (Category)


type alias Model =
    { name : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { name = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = NameInput String
    | Submit
    | SubmitResponse (Result Http.Error Category)


apiUrl : String
apiUrl =
    "http://localhost:8080/categories"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )

        Submit ->
            model ! [ submitForm model.name ]

        SubmitResponse res ->
            case res of
                Ok category ->
                    model ! []

                Err err ->
                    ( { model | error = Just "Error calling api" }, Cmd.none )


submitForm : String -> Cmd Msg
submitForm name =
    let
        category =
            Category 0 name

        req =
            Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/json" ]
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
    div []
        [ h2 [] [ text "Category" ]
        , input [ onInput NameInput ] []
        , button [ onClick Submit ] [ text "Save" ]
        ]
