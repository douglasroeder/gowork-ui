module Login exposing (..)

import Data.ApiResult exposing (ApiResult, apiErrorDecoder)
import Data.Authentication exposing (Authentication, decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Rest exposing (apiEndpoint)


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
    | HandleLoginResponse (WebData (ApiResult Authentication))


apiUrl : String
apiUrl =
    apiEndpoint ++ "/authenticate"


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        HandleLoginResponse res ->
            case res of
                Loading ->
                    ( model, Cmd.none, Nothing )

                Success data ->
                    ( initModel, Navigation.newUrl "#/", Just data.payload.token )

                Failure error ->
                    let
                        errMsg =
                            case error of
                                Http.BadStatus resp ->
                                    case resp.status.code of
                                        401 ->
                                            resp.body

                                        404 ->
                                            let
                                                body =
                                                    JD.decodeString apiErrorDecoder resp.body
                                            in
                                                case body of
                                                    Ok result ->
                                                        result.error

                                                    Err error ->
                                                        error

                                        _ ->
                                            resp.status.message

                                _ ->
                                    "Login Error!"
                    in
                        ( { model | error = Just errMsg }, Cmd.none, Nothing )

                NotAsked ->
                    ( model, Cmd.none, Nothing )

        Submit ->
            let
                body =
                    JE.object
                        [ ( "username", JE.string model.username )
                        , ( "password", JE.string model.password )
                        ]

                cmd =
                    RemoteData.Http.post apiUrl HandleLoginResponse decoder body
            in
                ( model, cmd, Nothing )

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
