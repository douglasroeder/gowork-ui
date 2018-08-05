module Page.Contacts exposing (Model, Msg, initModel, update, view)

import Html exposing (..)
import Data.Contact as Contact exposing (Contact)


type alias Model =
    { contacts : List Contact
    , errors : List String
    }


initModel : Model
initModel =
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
