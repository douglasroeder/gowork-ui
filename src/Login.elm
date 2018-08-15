module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Navigation


-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String
    | LoginResponse (Result Http.Error String)


url : String
url =
    "http://localhost:8080/authenticate"


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                body =
                    JE.object
                        [ ( "username", JE.string model.username )
                        , ( "password", JE.string model.password )
                        ]
                        |> JE.encode 4
                        |> Http.stringBody "application/json"

                decoder =
                    field "token" JD.string

                request =
                    Http.post url body decoder

                cmd =
                    Http.send LoginResponse request
            in
                ( model, cmd, Nothing )

        LoginResponse (Ok token) ->
            ( initModel, Navigation.newUrl "#/", Just token )

        LoginResponse (Err err) ->
            let
                errMsg =
                    case err of
                        Http.BadStatus resp ->
                            case resp.status.code of
                                401 ->
                                    resp.body

                                _ ->
                                    resp.status.message

                        _ ->
                            "Login Error!"
            in
                ( { model | error = Just errMsg }, Cmd.none, Nothing )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Nothing )



-- view


view : Model -> Html Msg
view model =
    div
        [ class "col-md-6" ]
        [ div [ class "bgc-white p-20 bd" ]
            [ h4 [ class "c-grey-900" ] [ text "Login" ]
            , div [ class "mT-30" ]
                [ errorPanel model.error
                , loginForm model
                ]
            ]
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ onSubmit Submit ]
        [ fieldset []
            [ div [ class "form-group" ]
                [ label [] [ text "User Name" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , class "form-control"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] []
                , button
                    [ class "btn btn-primary"
                    , type_ "submit"
                    ]
                    [ text "Login" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "alert alert-danger" ]
                [ text msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
