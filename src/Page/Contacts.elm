module Page.Contacts exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Data.Contact as Contact exposing (Contact)


type alias Model =
    { contacts : List Contact
    , errors : List String
    }


init : Model
init =
    { contacts = []
    , errors = []
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    text "Contact Page"
