module Page.Category exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = name }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Category" ]
        , input [ onInput NameInput ] []
        ]
